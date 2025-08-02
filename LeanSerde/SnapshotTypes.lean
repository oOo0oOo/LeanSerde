import Lean
import Lean.Elab.InfoTree
import Lean.Elab.Frontend
import LeanSerde.MetaTypes

open Lean Elab Command Term Meta Tactic

namespace LeanSerde

structure LeanContext where
  cmdState     : Option Command.State := none
  cmdContext   : Option Command.Context := none
  metaState    : Option Meta.State := none
  metaContext  : Option Meta.Context := none
  coreState    : Option Core.State := none
  coreContext  : Option Core.Context := none
  termState    : Option Term.State := none
  termContext  : Option Term.Context := none
  tacticState  : Option Tactic.State := none
  tacticContext: Option Tactic.Context := none
  deriving Inhabited

instance : Serializable LeanContext where
  encode ctx := do
    let timestamp := ← IO.monoMsNow
    let processId ← IO.Process.getCurrentDir >>= fun dir => return dir.toString.hash
    let path := s!"ctx_{timestamp}_{processId}.bin"
    saveModuleData path `ctx (unsafe unsafeCast ctx)
    let data ← IO.FS.readBinFile path
    IO.FS.removeFile path
    encode data
  decode sv := do
    let data : ByteArray ← decode sv
    let timestamp := ← IO.monoMsNow
    let processId ← IO.Process.getCurrentDir >>= fun dir => return dir.toString.hash
    let path := s!"ctx_{timestamp}_{processId}.bin"
    IO.FS.writeBinFile path data
    let (obj, _) ← readModuleData path
    IO.FS.removeFile path
    return (unsafe unsafeCast obj : LeanContext)

structure LeanSnapshot where
  env : Environment
  rootGoals : List MVarId := []
  ctx : LeanContext := {}
  deriving Serializable

namespace LeanSnapshot

private partial def extractGoalsFromTrees (trees : List InfoTree) : List (MetavarContext × MVarId) :=
  let rec findSorryInfo : InfoTree → List (MetavarContext × MVarId)
    | .context _ t' => findSorryInfo t'
    | .node (.ofTacticInfo tacInfo) children =>
      let goals := if tacInfo.stx.isOfKind ``Lean.Parser.Tactic.tacticSorry && !tacInfo.goalsBefore.isEmpty then
        [(tacInfo.mctxBefore, tacInfo.goalsBefore.head!)]
      else []
      goals ++ children.toList.flatMap findSorryInfo
    | .node _ children => children.toList.flatMap findSorryInfo
    | _ => []
  trees.flatMap findSorryInfo

private def processCommand (input : String) (cmdState? : Option Command.State) :
    IO (Command.State × List InfoTree) := unsafe do
  Lean.initSearchPath (← Lean.findSysroot)
  enableInitializersExecution
  let inputCtx := Parser.mkInputContext input "<input>"

  let commandState ← match cmdState? with
    | none => do
      let (header, _, messages) ← Parser.parseHeader inputCtx
      let (env, messages) ← processHeader header {} messages inputCtx
      pure (Command.mkState env messages {})
    | some state => pure state

  let commandState := { commandState with infoState.enabled := true }
  let s ← IO.processCommands inputCtx {} commandState
  let finalState := Frontend.State.commandState s
  pure (finalState, finalState.infoState.trees.toList)

private def withMeta (s : LeanSnapshot) (action : MetaM α) : IO (α × LeanSnapshot) := do
  let coreState := s.ctx.coreState.getD { env := s.env }
  let coreContext := s.ctx.coreContext.getD { fileName := "", fileMap := default }
  let metaState := s.ctx.metaState.getD {}
  let metaContext := s.ctx.metaContext.getD {}

  let ((result, newMetaState), newCoreState) ←
    (Core.CoreM.toIO · coreContext coreState) (Meta.MetaM.run action (ctx := metaContext) (s := metaState))

  return (result, { s with
    env := newCoreState.env,
    ctx := { s.ctx with
      coreState := some newCoreState,
      coreContext := some coreContext,
      metaState := some newMetaState,
      metaContext := some metaContext
    }
  })

def create (imports : List String := ["Init"]) : IO LeanSnapshot := do
  return { env := ← EnvironmentBuilder.fromImports imports }

def empty : IO LeanSnapshot := do
  return { env := ← EnvironmentBuilder.empty }

def fromEnv (env : Environment) : LeanSnapshot := { env }

def command (stmt : String) (s : LeanSnapshot) : IO LeanSnapshot := do
  let (newState, trees) ← processCommand stmt s.ctx.cmdState
  let goalData := extractGoalsFromTrees trees
  let (mctx, goalIds) := if goalData.isEmpty then (default, []) else
    (goalData.head!.fst, goalData.map (·.snd))

  return {
    env := newState.env,
    rootGoals := goalIds,
    ctx := { s.ctx with
      cmdState := some newState,
      cmdContext := s.ctx.cmdContext,
      metaState := some { mctx },
      coreState := some { env := newState.env }
    }
  }

def tactic (tacticStr : String) (s : LeanSnapshot) : IO LeanSnapshot := do
  if s.rootGoals.isEmpty then
    throw (IO.userError "No goals to apply tactic to")

  let stx ← match Parser.runParserCategory s.env `tactic tacticStr with
    | .error _ => throw (IO.userError s!"Parse error: {tacticStr}")
    | .ok stx => pure stx

  let ((goals, newTacticState, newTermState), s') ← s.withMeta (do
    let tacticState := s.ctx.tacticState.getD { goals := s.rootGoals }
    let tacticContext := s.ctx.tacticContext.getD { elaborator := .anonymous }
    let termContext := s.ctx.termContext.getD {}
    let termState := s.ctx.termState.getD {}

    let ((_, newTacticState), newTermState) ←
      evalTactic stx tacticContext |>.run tacticState |>.run (ctx := termContext) (s := termState)
    return (newTacticState.goals, newTacticState, newTermState)
  )

  return { s' with
    rootGoals := goals,
    ctx := { s'.ctx with
      tacticState := some newTacticState,
      termState := some newTermState
    }
  }

def goals (s : LeanSnapshot) : IO (List String) := do
  let (results, _) ← s.withMeta (s.rootGoals.mapM fun g => do
    let fmt ← Meta.ppGoal g
    return fmt.pretty)
  return results

def complete? (s : LeanSnapshot) : Bool := s.rootGoals.isEmpty

end LeanSnapshot
end LeanSerde
