import Lean
import Lean.Data.KVMap
import Lean.Meta.Diagnostics
import Lean.Environment
import LeanSerial.Derive
import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes
import LeanSerial.LibraryTypes

open Lean Environment

namespace LeanSerial

-- Towards Level
run_cmd mkSerializableInstance `Lean.Name
run_cmd mkSerializableInstance `Lean.LevelMVarId
run_cmd mkSerializableInstance `Lean.Level

-- Towards Syntax
run_cmd mkSerializableInstance `Lean.Syntax.Preresolved

instance : Serializable Lean.SourceInfo where
  encode si := match si with
    | SourceInfo.original leading pos trailing endPos =>
      .compound "SourceInfo.original" #[encode leading, encode pos, encode trailing, encode endPos]
    | SourceInfo.synthetic pos endPos useOriginalToStringPref =>
      .compound "SourceInfo.synthetic" #[encode pos, encode endPos, encode useOriginalToStringPref]
    | SourceInfo.none =>
      .compound "SourceInfo.none" #[]

  decode sv := do
    match sv with
    | .compound "SourceInfo.original" #[leading, pos, trailing, endPos] => do
      let leading ← decode leading
      let pos ← decode pos
      let trailing ← decode trailing
      let endPos ← decode endPos
      .ok (SourceInfo.original leading pos trailing endPos)
    | .compound "SourceInfo.synthetic" #[pos, endPos, useOriginalToStringPref] => do
      let pos ← decode pos
      let endPos ← decode endPos
      let useOriginalToStringPref ← decode useOriginalToStringPref
      .ok (SourceInfo.synthetic pos endPos useOriginalToStringPref)
    | .compound "SourceInfo.none" #[] =>
      .ok SourceInfo.none
    | .compound name args =>
      .error s!"Unknown SourceInfo constructor: {name} with {args.size} args"
    | other =>
      .error s!"Expected SourceInfo compound, got {repr other}"

-- deriving instance Serializable for Lean.Syntax  -- Takes forever!
run_cmd mkSerializableInstance `Lean.Syntax  -- Faster?

-- Towards Expr
run_cmd mkSerializableInstance `Lean.FVarId
run_cmd mkSerializableInstance `Lean.MVarId
run_cmd mkSerializableInstance `Lean.BinderInfo
run_cmd mkSerializableInstance `Lean.Literal
run_cmd mkSerializableInstance `Lean.DataValue

instance : Serializable Lean.KVMap where
  encode m := .compound "KVMap" (m.entries.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v]) |>.toArray)
  decode sv := do
    let args ← decodeCompound "KVMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok ⟨entries.toList⟩

run_cmd mkSerializableInstance `Lean.Expr

-- Towards MetavarContext
run_cmd mkSerializableInstance `Lean.LocalDeclKind
run_cmd mkSerializableInstance `Lean.LocalDecl

instance {α : Type} [Serializable α] : Serializable (Lean.FVarIdMap α) :=
  inferInstanceAs (Serializable (Lean.RBMap Lean.FVarId α (Name.quickCmp ·.name ·.name)))

run_cmd mkSerializableInstance `Lean.LocalContext
run_cmd mkSerializableInstance `Lean.MetavarKind
run_cmd mkSerializableInstance `Lean.LocalInstance
run_cmd mkSerializableInstance `Lean.MetavarDecl
run_cmd mkSerializableInstance `Lean.DelayedMetavarAssignment
run_cmd mkSerializableInstance `Lean.MetavarContext

-- Towards ConstantInfo
run_cmd mkSerializableInstance `Lean.ConstantVal
run_cmd mkSerializableInstance `Lean.AxiomVal
run_cmd mkSerializableInstance `Lean.ReducibilityHints
run_cmd mkSerializableInstance `Lean.DefinitionSafety
run_cmd mkSerializableInstance `Lean.TheoremVal
run_cmd mkSerializableInstance `Lean.OpaqueVal
run_cmd mkSerializableInstance `Lean.QuotKind
run_cmd mkSerializableInstance `Lean.QuotVal
run_cmd mkSerializableInstance `Lean.ConstructorVal
run_cmd mkSerializableInstance `Lean.InductiveVal
run_cmd mkSerializableInstance `Lean.DefinitionVal
run_cmd mkSerializableInstance `Lean.RecursorRule
run_cmd mkSerializableInstance `Lean.RecursorVal
run_cmd mkSerializableInstance `Lean.ConstantInfo


-- Towards Environment (only partial serialization and deserialization)
instance : Serializable ModuleIdx where
  encode idx := .nat idx.toNat
  decode := fun | .nat n => .ok n | other => .error s!"Expected ModuleIdx, got {repr other}"

run_cmd mkSerializableInstance `Lean.Import

instance [Serializable α] [Serializable β] [BEq α] [Hashable α] : Serializable (Lean.SMap α β) where
  encode m := .compound "SMap" (m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v]) |>.toArray)
  decode sv := do
    let args ← decodeCompound "SMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) Lean.SMap.empty)

instance : Serializable Lean.NameSet where
  encode s := .compound "NameSet" (s.toList.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "NameSet" sv
    let names ← args.mapM decode
    .ok (names.toList.foldl (fun acc name => acc.insert name) Lean.NameSet.empty)

instance : Serializable (NameMap String) where
  encode m := .compound "NameMapString" (m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v]) |>.toArray)
  decode sv := do
    let args ← decodeCompound "NameMapString" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) {})

instance : Serializable Kernel.Diagnostics where
  encode diag := .compound "Diagnostics" #[
    encode diag.unfoldCounter,
    encode diag.enabled
  ]
  decode sv := do
    let args ← decodeCompound "Diagnostics" sv
    if args.size = 2 then do
      let unfoldCounter ← decode args[0]!
      let enabled ← decode args[1]!
      .ok { unfoldCounter, enabled }
    else
      .error s!"Diagnostics expects 2 args, got {args.size}"

instance : Serializable Lean.Options where
  encode := encode (α := Lean.KVMap)
  decode := decode (α := Lean.KVMap)

instance : Serializable Lean.ModuleData where
  encode md := .compound "ModuleData" #[
    encode md.isModule,
    encode md.imports,
    encode md.constNames,
    encode md.constants,
    encode md.extraConstNames,
    -- Skip md.entries: (Array (Name × Array EnvExtensionEntry))
  ]
  decode sv := do
    let args ← decodeCompound "ModuleData" sv
    if args.size = 5 then do
      let isModule ← decode args[0]!
      let imports ← decode args[1]!
      let constNames ← decode args[2]!
      let constants ← decode args[3]!
      let extraConstNames ← decode args[4]!
      .ok {
        isModule,
        imports,
        constNames,
        constants,
        extraConstNames,
        entries := Array.empty  -- Not serialized
      }
    else
      .error s!"ModuleData expects 5 args, got {args.size}"

instance : Serializable Lean.CompactedRegion where
  encode r := .nat r.toNat
  decode := fun
    | .nat n => .ok (USize.ofNat n)
    | other => .error s!"Expected CompactedRegion, got {repr other}"

run_cmd mkSerializableInstance `Lean.EnvironmentHeader

-- Can only serialize a subset of Environment
structure EnvironmentData where
  constants : Array ConstantInfo
  const2ModIdx : Std.HashMap Name ModuleIdx
  quotInit : Bool
  diagnostics : Kernel.Diagnostics
  header : EnvironmentHeader
  deriving LeanSerial.Serializable

-- Helper function to add constant (following Lake pattern)
@[extern "lake_environment_add"]
private opaque lakeEnvironmentAdd (env : Environment) (_ : ConstantInfo) : Environment

-- Environment serialization using monad lifting tricks (too hacky?)
instance : Serializable Lean.Environment where
  encode env :=
    let kenv := env.toKernelEnv
    encode ({
      constants := kenv.constants.foldStage2 (fun cs _ c => cs.push c) #[],
      const2ModIdx := kenv.const2ModIdx,
      quotInit := kenv.quotInit,
      diagnostics := kenv.diagnostics,
      header := kenv.header
    } : EnvironmentData)

  decode sv := do
    let data ← decode (α := EnvironmentData) sv
    let ioAction : IO Environment := do
      let env ← mkEmptyEnvironment data.header.trustLevel
      let env := env.setMainModule data.header.mainModule
      let finalEnv := data.constants.foldl lakeEnvironmentAdd env
      return finalEnv
    let baseIOAction : BaseIO (Except IO.Error Environment) := ioAction.toBaseIO
    match baseIOAction.run ⟨⟩ with
    | EStateM.Result.ok (Except.ok env) _ => .ok env
    | EStateM.Result.ok (Except.error _) _ => .error "IO.Error during environment creation"
    | EStateM.Result.error _ _ => .error "Failed to create environment"

-- Towards InfoTree

-- StateM & Dynamic: Encode type information only
instance {σ α} [Inhabited α] : Serializable (StateM σ α) where
  encode _ := .compound "StateM.phantom" #[]
  decode _ := .ok (fun s => (default, s))

deriving instance TypeName for String
instance : Serializable Dynamic where
  encode _ := .compound "Dynamic.phantom" #[]
  decode _ := .ok (Dynamic.mk "phantom")

run_cmd mkSerializableInstance `Lean.Widget.WidgetInstance
run_cmd mkSerializableInstance `Lean.DeclarationRange
run_cmd mkSerializableInstance `Lean.DeclarationLocation
run_cmd mkSerializableInstance `Lean.Elab.MacroExpansionInfo
run_cmd mkSerializableInstance `Lean.Elab.FieldInfo
run_cmd mkSerializableInstance `Lean.Elab.OptionInfo
run_cmd mkSerializableInstance `Lean.Elab.FieldRedeclInfo
run_cmd mkSerializableInstance `Lean.Elab.FVarAliasInfo
run_cmd mkSerializableInstance `Lean.Elab.UserWidgetInfo
run_cmd mkSerializableInstance `Lean.Elab.CustomInfo
run_cmd mkSerializableInstance `Lean.Elab.ElabInfo
run_cmd mkSerializableInstance `Lean.Elab.ChoiceInfo
run_cmd mkSerializableInstance `Lean.Elab.PartialTermInfo
run_cmd mkSerializableInstance `Lean.Elab.TermInfo
run_cmd mkSerializableInstance `Lean.Elab.CompletionInfo
run_cmd mkSerializableInstance `Lean.Elab.DelabTermInfo
run_cmd mkSerializableInstance `Lean.Elab.TacticInfo
run_cmd mkSerializableInstance `Lean.Elab.CommandInfo
run_cmd mkSerializableInstance `Lean.Elab.Info
run_cmd mkSerializableInstance `Lean.NameGenerator
run_cmd mkSerializableInstance `Lean.OpenDecl
run_cmd mkSerializableInstance `Lean.FileMap
run_cmd mkSerializableInstance `Lean.Elab.CommandContextInfo
run_cmd mkSerializableInstance `Lean.Elab.PartialContextInfo

partial def encodeInfoTree (it : Lean.Elab.InfoTree) : SerialValue :=
  match it with
  | .context i t => .compound "InfoTree.context" #[encode i, encodeInfoTree t]
  | .node i children => .compound "InfoTree.node" #[encode i, .compound "Array" (children.toArray.map encodeInfoTree)]
  | .hole mvarId => .compound "InfoTree.hole" #[encode mvarId]

partial def decodeInfoTree (sv : SerialValue) : Except String Lean.Elab.InfoTree := do
  match sv with
  | .compound "InfoTree.context" #[i, t] =>
    let i ← decode i
    let t ← decodeInfoTree t
    .ok (.context i t)
  | .compound "InfoTree.node" #[i, .compound "Array" childrenSvs] =>
    let i ← decode i
    let childrenDecoded ← childrenSvs.mapM decodeInfoTree
    let childrenPersistent := childrenDecoded.foldl (fun acc child => acc.push child) Lean.PersistentArray.empty
    .ok (.node i childrenPersistent)
  | .compound "InfoTree.hole" #[mvarId] =>
    let mvarId ← decode mvarId
    .ok (.hole mvarId)
  | _ => .error s!"Expected InfoTree compound, got {repr sv}"

instance : Serializable Lean.Elab.InfoTree where
  encode := encodeInfoTree
  decode := decodeInfoTree

-- Towards Tactic.SavedState
instance {α : Type} [Serializable α] : Serializable (Lean.MVarIdMap α) :=
  inferInstanceAs (Serializable (Lean.RBMap Lean.MVarId α (Name.quickCmp ·.name ·.name)))

instance : Serializable Lean.FVarIdSet :=
  inferInstanceAs (Serializable (Lean.RBTree Lean.FVarId (Name.quickCmp ·.name ·.name)))

instance : Serializable Lean.MVarIdSet :=
  inferInstanceAs (Serializable (Lean.RBTree Lean.MVarId (Name.quickCmp ·.name ·.name)))

instance {ks : SyntaxNodeKinds} : Serializable (TSyntax ks) where
  encode t := encode t.raw
  decode sv := do
    let syn ← decode sv
    .ok ⟨syn⟩

instance {ks : SyntaxNodeKinds} : Serializable (TSyntaxArray ks) :=
  inferInstanceAs (Serializable (Array (TSyntax ks)))

-- run_cmd mkSerializableInstance `Lean.Format
-- run_cmd mkSerializableInstance `Lean.Message
-- run_cmd mkSerializableInstance `IO.Ref
-- run_cmd mkSerializableInstance `Lean.Meta.FunInfoCache
-- run_cmd mkSerializableInstance `Lean.Meta.InferTypeCache
-- run_cmd mkSerializableInstance `Lean.Meta.SynthInstanceCache
-- run_cmd mkSerializableInstance `Lean.Elab.MacroStack
-- run_cmd mkSerializableInstance `Lean.Term

-- run_cmd mkSerializableInstance `Lean.Elab.PartialFixpoint.PartialFixpointType
-- run_cmd mkSerializableInstance `Lean.Elab.PartialFixpoint.EqnInfo
-- run_cmd mkSerializableInstance `Lean.Elab.PartialFixpoint.FixedParamPerm
-- run_cmd mkSerializableInstance `Lean.Elab.PartialFixpoint.FixedParamPerms
-- run_cmd mkSerializableInstance `Lean.Elab.PartialFixpoint

-- run_cmd mkSerializableInstance `Lean.Elab.DecreasingBy
-- run_cmd mkSerializableInstance `Lean.Elab.TerminationBy
-- run_cmd mkSerializableInstance `Lean.AttributeKind
-- run_cmd mkSerializableInstance `Lean.TraceData
-- run_cmd mkSerializableInstance `Lean.NamingContext
-- run_cmd mkSerializableInstance `Lean.Meta.DefEqContext
-- run_cmd mkSerializableInstance `Lean.Elab.TerminationHints
-- run_cmd mkSerializableInstance `Lean.Elab.Attribute
-- run_cmd mkSerializableInstance `Lean.FormatWithInfos
-- run_cmd mkSerializableInstance `Lean.MessageDataContext
-- run_cmd mkSerializableInstance `Lean.MessageData
-- run_cmd mkSerializableInstance `Lean.Elab.Term.MVarErrorKind
-- run_cmd mkSerializableInstance `Lean.TraceElem
-- run_cmd mkSerializableInstance `Lean.Elab.Term.TacticMVarKind
-- run_cmd mkSerializableInstance `Lean.Elab.Term.SavedContext
-- run_cmd mkSerializableInstance `Lean.Meta.Diagnostics
-- run_cmd mkSerializableInstance `Lean.MessageLog
-- run_cmd mkSerializableInstance `Lean.Meta.PostponedEntry
-- run_cmd mkSerializableInstance `Lean.Meta.Cache
-- run_cmd mkSerializableInstance `Lean.TraceState
-- run_cmd mkSerializableInstance `Lean.Language.Snapshot.Diagnostics
-- run_cmd mkSerializableInstance `Lean.Language.Snapshot
-- run_cmd mkSerializableInstance `Lean.Language.SnapshotTree
-- run_cmd mkSerializableInstance `Lean.Language.SnapshotTask
-- run_cmd mkSerializableInstance `Lean.Elab.InfoState
-- run_cmd mkSerializableInstance `Lean.Core.Cache
-- run_cmd mkSerializableInstance `Lean.Elab.Term.LetRecToLift
-- run_cmd mkSerializableInstance `Lean.Elab.Term.LevelMVarErrorInfo
-- run_cmd mkSerializableInstance `Lean.Elab.Term.MVarErrorInfo
-- run_cmd mkSerializableInstance `Lean.Elab.Term.SyntheticMVarKind
-- run_cmd mkSerializableInstance `Lean.Elab.Term.SyntheticMVarDecl
-- run_cmd mkSerializableInstance `Lean.DeclNameGenerator
-- run_cmd mkSerializableInstance `Lean.Meta.State
-- run_cmd mkSerializableInstance `Lean.Core.State
-- run_cmd mkSerializableInstance `Lean.Core.SavedState
-- run_cmd mkSerializableInstance `Lean.Elab.Term.State
-- run_cmd mkSerializableInstance `Lean.Meta.SavedState
-- run_cmd mkSerializableInstance `Lean.Elab.Tactic.State
-- run_cmd mkSerializableInstance `Lean.Elab.Term.SavedState
-- run_cmd mkSerializableInstance `Lean.Elab.Tactic.SavedState

-- Various
run_cmd mkSerializableInstance `Lean.Widget.UserWidgetDefinition

end LeanSerial
