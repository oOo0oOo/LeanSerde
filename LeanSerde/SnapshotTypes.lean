import Lean
import Lean.Elab.InfoTree
import Lean.Elab.Frontend
import LeanSerde.MetaTypes

open Lean Elab Command Term Meta Tactic

namespace LeanSerde

def contains (s : String) (sub : String) : Bool :=
  let parts := s.splitOn sub
  parts.length > 1

namespace Lean.Elab.IO

def processCommandsWithInfoTrees
    (inputCtx : Parser.InputContext) (parserState : Parser.ModuleParserState)
    (commandState : Command.State) : IO (Command.State × List Message × List InfoTree) := do
  let commandState := { commandState with infoState.enabled := true }
  let s ← IO.processCommands inputCtx parserState commandState <&> Frontend.State.commandState
  pure (s, s.messages.toList, s.infoState.trees.toList)

def processInput (input : String) (cmdState? : Option Command.State)
    (opts : Options := {}) (fileName : Option String := none) :
    IO (Command.State × Command.State × List Message × List InfoTree) := unsafe do
  Lean.initSearchPath (← Lean.findSysroot)
  enableInitializersExecution
  let fileName   := fileName.getD "<input>"
  let inputCtx   := Parser.mkInputContext input fileName

  match cmdState? with
  | none => do
    let (header, parserState, messages) ← Parser.parseHeader inputCtx
    let (env, messages) ← processHeader header opts messages inputCtx
    let headerOnlyState := Command.mkState env messages opts
    let (cmdState, messages, trees) ← processCommandsWithInfoTrees inputCtx parserState headerOnlyState
    return (headerOnlyState, cmdState, messages, trees)

  | some cmdStateBefore => do
    let parserState : Parser.ModuleParserState := {}
    let (cmdStateAfter, messages, trees) ← processCommandsWithInfoTrees inputCtx parserState cmdStateBefore
    return (cmdStateBefore, cmdStateAfter, messages, trees)

end Lean.Elab.IO

private def isSorryTactic (stx : Syntax) : Bool :=
  stx.isOfKind ``Lean.Parser.Tactic.tacticSorry

private def isSorryTerm (stx : Syntax) : Bool :=
  stx.isOfKind ``Lean.Parser.Term.sorry

namespace Lean.Elab.InfoTree

inductive SorryType
| tactic : MVarId → SorryType
| term : LocalContext → Option Expr → SorryType
deriving Inhabited

def stxRange (fileMap : FileMap) (stx : Syntax) : Position × Position :=
  let pos    := stx.getPos?.getD 0
  let endPos := stx.getTailPos?.getD pos
  (fileMap.toPosition pos, fileMap.toPosition endPos)

partial def findAllInfo (t : InfoTree) (ctx? : Option ContextInfo) (p : Info → Bool)
    (stop : Info → Bool := fun _ => false) :
    List (Info × Option ContextInfo) :=
  match t with
  | .context ctx t => findAllInfo t (ctx.mergeIntoOuter? ctx?) p stop
  | .node i ts  =>
    let info := if p i then [(i, ctx?)] else []
    let rest := if stop i then [] else ts.toList.flatMap (fun t => findAllInfo t ctx? p stop)
    info ++ rest
  | _ => []

def findSorryTacticNodes (t : InfoTree) : List (TacticInfo × ContextInfo) :=
  let infos := findAllInfo t none fun i => match i with
  | .ofTacticInfo i => isSorryTactic i.stx && !i.goalsBefore.isEmpty
  | _ => false
  infos.filterMap fun p => match p with
  | (.ofTacticInfo i, some ctx) => some (i, ctx)
  | _ => none

def findSorryTermNodes (t : InfoTree) : List (TermInfo × ContextInfo) :=
  let infos := findAllInfo t none
    (fun i => match i with | .ofTermInfo i => isSorryTerm i.stx | _ => false)
    (fun i => match i with | .ofTacticInfo i => isSorryTactic i.stx | _ => false)
  infos.filterMap fun p => match p with
  | (.ofTermInfo i, some ctx) => some (i, ctx)
  | _ => none

def sorries (t : InfoTree) : List (ContextInfo × SorryType × Position × Position) :=
  (findSorryTacticNodes t |>.map fun (i, ctx) =>
    ({ ctx with mctx := i.mctxBefore, ngen := ctx.ngen.mkChild.1 }, .tactic i.goalsBefore.head!,
      stxRange ctx.fileMap i.stx)) ++
  (findSorryTermNodes t |>.map fun (i, ctx) =>
    (ctx, .term i.lctx i.expectedType?, stxRange ctx.fileMap i.stx))

end Lean.Elab.InfoTree

structure LeanContext where
  cmdState     : Option Command.State := none
  cmdContext   : Option Command.Context := none
  coreState    : Option Core.State := none
  coreContext  : Option Core.Context := none
  metaState    : Option Meta.State := none
  metaContext  : Option Meta.Context := none
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

def create (imports : List String := ["Init"]) : IO LeanSnapshot := do
  let env ← EnvironmentBuilder.fromImports imports
  return { env, rootGoals := [], ctx := {} }

def empty : IO LeanSnapshot := do
  let env ← EnvironmentBuilder.empty
  return { env, rootGoals := [], ctx := {} }

def fromEnv (env : Environment) : LeanSnapshot :=
  { env, rootGoals := [], ctx := {} }

def hasGoals (s : LeanSnapshot) : Bool := !s.rootGoals.isEmpty

private def runCore (s : LeanSnapshot) (t : CoreM α) : IO (α × LeanSnapshot) := do
  let coreState := s.ctx.coreState.getD { env := s.env }
  let coreContext := s.ctx.coreContext.getD { fileName := "", fileMap := default }
  let (a, newCoreState) ← (Core.CoreM.toIO · coreContext coreState) t
  return (a, { s with ctx := { s.ctx with coreState := some newCoreState } })

private def runMeta (s : LeanSnapshot) (t : MetaM α) : IO (α × LeanSnapshot) := do
  let metaState := s.ctx.metaState.getD { mctx := {} }
  let metaContext := s.ctx.metaContext.getD {}
  let ((a, newMetaState), s') ← s.runCore (Meta.MetaM.run (ctx := metaContext) (s := metaState) t)
  return (a, { s' with ctx := { s'.ctx with metaState := some newMetaState } })

private def runTerm (s : LeanSnapshot) (t : TermElabM α) : IO (α × LeanSnapshot) := do
  let termState := s.ctx.termState.getD {}
  let termContext := s.ctx.termContext.getD {}
  let ((a, newTermState), s') ← s.runMeta (Term.TermElabM.run (ctx := termContext) (s := termState) t)
  return (a, { s' with ctx := { s'.ctx with termState := some newTermState } })

private def runTacticM (s : LeanSnapshot) (t : TacticM α) : IO (α × LeanSnapshot) := do
  let some tacticState := s.ctx.tacticState | throw (IO.userError "No tactic state")
  let some tacticContext := s.ctx.tacticContext | throw (IO.userError "No tactic context")
  let ((a, newTacticState), s') ← s.runTerm (t tacticContext |>.run tacticState)
  return (a, { s' with
    rootGoals := newTacticState.goals,
    ctx := { s'.ctx with tacticState := some newTacticState }
  })

private def runSyntax (s : LeanSnapshot) (stx : Syntax) : IO LeanSnapshot :=
  (s.runTacticM (evalTactic stx)).map (·.2)

def command (stmt : String) (s : LeanSnapshot) : IO LeanSnapshot := do
  let (_, newState, _, trees) ← Lean.Elab.IO.processInput stmt s.ctx.cmdState

  let sorries := trees.flatMap Lean.Elab.InfoTree.sorries
  let newGoals := sorries.filterMap fun (ctxInfo, goal, _, _) =>
    match goal with
    | Lean.Elab.InfoTree.SorryType.tactic mvarId => some (ctxInfo.mctx, mvarId)
    | _ => none

  -- Use the MetavarContext from the first goal if available
  let (finalMctx, goalIds) :=
    if newGoals.isEmpty then
      ({}, [])  -- Empty MetavarContext and no goals
    else
      let (firstMctx, firstGoal) := newGoals.head!
      (firstMctx, newGoals.map (·.2))

  return {
    env := newState.env,
    rootGoals := goalIds,
    ctx := {
      cmdState := some newState,
      metaState := some { mctx := finalMctx },
      metaContext := some {},
      coreState := some { env := newState.env },
      coreContext := some { fileName := "", fileMap := default }
    }
  }

def tactic (tacticStr : String) (s : LeanSnapshot) : IO LeanSnapshot := do
  if s.rootGoals.isEmpty then
    throw (IO.userError "No goals to apply tactic to")

  -- Ensure we have the proper context with the right MetavarContext
  let metaState := s.ctx.metaState.getD { mctx := {} }
  let s := { s with ctx := { s.ctx with
      tacticState := some { goals := s.rootGoals },
      tacticContext := some { elaborator := .anonymous },
      metaState := some metaState
    }
  }

  match Parser.runParserCategory s.env `tactic tacticStr with
  | .error _ => throw (IO.userError s!"Parse error: {tacticStr}")
  | .ok stx => s.runSyntax stx

def goals (s : LeanSnapshot) : IO (List String) := do
  let (results, _) ← s.runMeta (s.rootGoals.mapM fun g => do
    let fmt ← Meta.ppGoal g
    return fmt.pretty)
  return results

def complete? (s : LeanSnapshot) : Bool := s.rootGoals.isEmpty

end LeanSnapshot
end LeanSerde
