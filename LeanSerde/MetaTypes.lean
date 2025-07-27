import Lean
import Lean.Data.KVMap
import Lean.Meta.Diagnostics
import Lean.Environment
import LeanSerde.Derive
import LeanSerde.PrimitiveTypes
import LeanSerde.ContainerTypes
import LeanSerde.LibraryTypes

open Lean Environment

namespace LeanSerde

-- Towards Level
deriving instance LeanSerde.Serializable for Lean.Name, Lean.LevelMVarId, Lean.Level

-- Towards Syntax
deriving instance LeanSerde.Serializable for Lean.Syntax.Preresolved

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

deriving instance LeanSerde.Serializable for Lean.Syntax  -- Mega slow

-- Towards Expr
deriving instance LeanSerde.Serializable for Lean.FVarId, Lean.MVarId, Lean.BinderInfo, Lean.Literal, Lean.DataValue

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

deriving instance LeanSerde.Serializable for Lean.Expr

-- Towards MetavarContext
deriving instance LeanSerde.Serializable for Lean.LocalDeclKind, Lean.LocalDecl

instance {α : Type} [Serializable α] : Serializable (Lean.FVarIdMap α) :=
  inferInstanceAs (Serializable (Lean.RBMap Lean.FVarId α (Name.quickCmp ·.name ·.name)))

deriving instance LeanSerde.Serializable for Lean.LocalContext, Lean.MetavarKind, Lean.LocalInstance
deriving instance LeanSerde.Serializable for Lean.MetavarDecl, Lean.DelayedMetavarAssignment, Lean.MetavarContext

-- Towards ConstantInfo
deriving instance LeanSerde.Serializable for Lean.ConstantVal, Lean.AxiomVal, Lean.ReducibilityHints, Lean.DefinitionSafety
deriving instance LeanSerde.Serializable for Lean.TheoremVal, Lean.OpaqueVal, Lean.QuotKind, Lean.QuotVal
deriving instance LeanSerde.Serializable for Lean.ConstructorVal, Lean.InductiveVal, Lean.DefinitionVal
deriving instance LeanSerde.Serializable for Lean.RecursorRule, Lean.RecursorVal, Lean.ConstantInfo


-- Towards Environment (only partial serialization and deserialization)
instance : Serializable ModuleIdx where
  encode idx := .nat idx.toNat
  decode := fun | .nat n => .ok n | other => .error s!"Expected ModuleIdx, got {repr other}"

deriving instance LeanSerde.Serializable for Lean.Import

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

deriving instance LeanSerde.Serializable for Lean.EnvironmentHeader

-- Can only serialize a subset of Environment
structure EnvironmentData where
  constants : Array ConstantInfo
  const2ModIdx : Std.HashMap Name ModuleIdx
  quotInit : Bool
  diagnostics : Kernel.Diagnostics
  header : EnvironmentHeader
  deriving LeanSerde.Serializable

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

deriving instance LeanSerde.Serializable for Lean.Widget.WidgetInstance, Lean.DeclarationRange
deriving instance LeanSerde.Serializable for Lean.DeclarationLocation, Lean.Elab.MacroExpansionInfo
deriving instance LeanSerde.Serializable for Lean.Elab.FieldInfo, Lean.Elab.OptionInfo
deriving instance LeanSerde.Serializable for Lean.Elab.FieldRedeclInfo, Lean.Elab.FVarAliasInfo
deriving instance LeanSerde.Serializable for Lean.Elab.UserWidgetInfo, Lean.Elab.CustomInfo
deriving instance LeanSerde.Serializable for Lean.Elab.ElabInfo, Lean.Elab.ChoiceInfo
deriving instance LeanSerde.Serializable for Lean.Elab.PartialTermInfo, Lean.Elab.TermInfo
deriving instance LeanSerde.Serializable for Lean.Elab.CompletionInfo, Lean.Elab.DelabTermInfo
deriving instance LeanSerde.Serializable for Lean.Elab.TacticInfo, Lean.Elab.CommandInfo
deriving instance LeanSerde.Serializable for Lean.Elab.Info, Lean.NameGenerator
deriving instance LeanSerde.Serializable for Lean.OpenDecl, Lean.FileMap
deriving instance LeanSerde.Serializable for Lean.Elab.CommandContextInfo, Lean.Elab.PartialContextInfo

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

-- deriving instance LeanSerde.Serializable for Lean.Format
-- deriving instance LeanSerde.Serializable for Lean.Message
-- deriving instance LeanSerde.Serializable for IO.Ref
-- deriving instance LeanSerde.Serializable for Lean.Meta.FunInfoCache
-- deriving instance LeanSerde.Serializable for Lean.Meta.InferTypeCache
-- deriving instance LeanSerde.Serializable for Lean.Meta.SynthInstanceCache
-- deriving instance LeanSerde.Serializable for Lean.Elab.MacroStack
-- deriving instance LeanSerde.Serializable for Lean.Term

-- deriving instance LeanSerde.Serializable for Lean.Elab.PartialFixpoint.PartialFixpointType
-- deriving instance LeanSerde.Serializable for Lean.Elab.PartialFixpoint.EqnInfo
-- deriving instance LeanSerde.Serializable for Lean.Elab.PartialFixpoint.FixedParamPerm
-- deriving instance LeanSerde.Serializable for Lean.Elab.PartialFixpoint.FixedParamPerms
-- deriving instance LeanSerde.Serializable for Lean.Elab.PartialFixpoint

-- deriving instance LeanSerde.Serializable for Lean.Elab.DecreasingBy
-- deriving instance LeanSerde.Serializable for Lean.Elab.TerminationBy
-- deriving instance LeanSerde.Serializable for Lean.AttributeKind
-- deriving instance LeanSerde.Serializable for Lean.TraceData
-- deriving instance LeanSerde.Serializable for Lean.NamingContext
-- deriving instance LeanSerde.Serializable for Lean.Meta.DefEqContext
-- deriving instance LeanSerde.Serializable for Lean.Elab.TerminationHints
-- deriving instance LeanSerde.Serializable for Lean.Elab.Attribute
-- deriving instance LeanSerde.Serializable for Lean.FormatWithInfos
-- deriving instance LeanSerde.Serializable for Lean.MessageDataContext
-- deriving instance LeanSerde.Serializable for Lean.MessageData
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.MVarErrorKind
-- deriving instance LeanSerde.Serializable for Lean.TraceElem
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.TacticMVarKind
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.SavedContext
-- deriving instance LeanSerde.Serializable for Lean.Meta.Diagnostics
-- deriving instance LeanSerde.Serializable for Lean.MessageLog
-- deriving instance LeanSerde.Serializable for Lean.Meta.PostponedEntry
-- deriving instance LeanSerde.Serializable for Lean.Meta.Cache
-- deriving instance LeanSerde.Serializable for Lean.TraceState
-- deriving instance LeanSerde.Serializable for Lean.Language.Snapshot.Diagnostics
-- deriving instance LeanSerde.Serializable for Lean.Language.Snapshot
-- deriving instance LeanSerde.Serializable for Lean.Language.SnapshotTree
-- deriving instance LeanSerde.Serializable for Lean.Language.SnapshotTask
-- deriving instance LeanSerde.Serializable for Lean.Elab.InfoState
-- deriving instance LeanSerde.Serializable for Lean.Core.Cache
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.LetRecToLift
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.LevelMVarErrorInfo
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.MVarErrorInfo
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.SyntheticMVarKind
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.SyntheticMVarDecl
-- deriving instance LeanSerde.Serializable for Lean.DeclNameGenerator
-- deriving instance LeanSerde.Serializable for Lean.Meta.State
-- deriving instance LeanSerde.Serializable for Lean.Core.State
-- deriving instance LeanSerde.Serializable for Lean.Core.SavedState
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.State
-- deriving instance LeanSerde.Serializable for Lean.Meta.SavedState
-- deriving instance LeanSerde.Serializable for Lean.Elab.Tactic.State
-- deriving instance LeanSerde.Serializable for Lean.Elab.Term.SavedState
-- deriving instance LeanSerde.Serializable for Lean.Elab.Tactic.SavedState

-- Various
deriving instance LeanSerde.Serializable for Lean.Widget.UserWidgetDefinition

end LeanSerde
