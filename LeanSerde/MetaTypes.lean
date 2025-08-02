import Lean
import Lean.Replay
import Lean.Data.KVMap
import Lean.Meta.Diagnostics
import Lean.Environment
import LeanSerde.Derive
import LeanSerde.PrimitiveTypes
import LeanSerde.ContainerTypes
import LeanSerde.LibraryTypes

open Lean Elab Command Term Meta Tactic

namespace LeanSerde

-- Towards Level
deriving instance LeanSerde.Serializable for Lean.Name, Lean.LevelMVarId, Lean.Level

-- Towards Syntax
deriving instance LeanSerde.Serializable for Lean.Syntax.Preresolved

instance : Serializable Lean.SourceInfo where
  encode si := match si with
    | SourceInfo.original leading pos trailing endPos => do
      let encodedLeading ← encode leading
      let encodedPos ← encode pos
      let encodedTrailing ← encode trailing
      let encodedEndPos ← encode endPos
      return .compound "SourceInfo.original" #[encodedLeading, encodedPos, encodedTrailing, encodedEndPos]
    | SourceInfo.synthetic pos endPos useOriginalToStringPref => do
      let encodedPos ← encode pos
      let encodedEndPos ← encode endPos
      let encodedPref ← encode useOriginalToStringPref
      return .compound "SourceInfo.synthetic" #[encodedPos, encodedEndPos, encodedPref]
    | SourceInfo.none =>
      return .compound "SourceInfo.none" #[]

  decode sv := do
    match sv with
    | .compound "SourceInfo.original" #[leading, pos, trailing, endPos] => do
      let leading ← decode leading
      let pos ← decode pos
      let trailing ← decode trailing
      let endPos ← decode endPos
      return (SourceInfo.original leading pos trailing endPos)
    | .compound "SourceInfo.synthetic" #[pos, endPos, useOriginalToStringPref] => do
      let pos ← decode pos
      let endPos ← decode endPos
      let useOriginalToStringPref ← decode useOriginalToStringPref
      return (SourceInfo.synthetic pos endPos useOriginalToStringPref)
    | .compound "SourceInfo.none" #[] =>
      return SourceInfo.none
    | .compound name args =>
      throw s!"Unknown SourceInfo constructor: {name} with {args.size} args"
    | other =>
      throw s!"Expected SourceInfo compound, got {repr other}"

deriving instance LeanSerde.Serializable for Lean.Syntax  -- Mega slow

-- Towards Expr
deriving instance LeanSerde.Serializable for Lean.FVarId, Lean.MVarId, Lean.BinderInfo, Lean.Literal, Lean.DataValue

instance : Serializable Lean.KVMap where
  encode m := do
    let encodedEntries ← m.entries.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "KVMap" encodedEntries.toArray
  decode sv := do
    let args ← decodeCompound "KVMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return ⟨entries.toList⟩

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
  encode idx := return .nat idx.toNat
  decode := fun | .nat n => return n | other => throw s!"Expected ModuleIdx, got {repr other}"

deriving instance LeanSerde.Serializable for Lean.Import

instance [Serializable α] [Serializable β] [BEq α] [Hashable α] : Serializable (Lean.SMap α β) where
  encode m := do
    let encodedEntries ← m.toList.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "SMap" encodedEntries.toArray
  decode sv := do
    let args ← decodeCompound "SMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) Lean.SMap.empty)

instance : Serializable Lean.NameSet where
  encode s := do
    let encodedNames ← s.toList.mapM encode
    return .compound "NameSet" encodedNames.toArray
  decode sv := do
    let args ← decodeCompound "NameSet" sv
    let names ← args.mapM decode
    return (names.toList.foldl (fun acc name => acc.insert name) Lean.NameSet.empty)

instance : Serializable (NameMap String) where
  encode m := do
    let encodedEntries ← m.toList.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "NameMapString" encodedEntries.toArray
  decode sv := do
    let args ← decodeCompound "NameMapString" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) {})

instance : Serializable Kernel.Diagnostics where
  encode diag := do
    let encodedCounter ← encode diag.unfoldCounter
    let encodedEnabled ← encode diag.enabled
    return .compound "Diagnostics" #[encodedCounter, encodedEnabled]
  decode sv := do
    let args ← decodeCompound "Diagnostics" sv
    if args.size = 2 then do
      let unfoldCounter ← decode args[0]!
      let enabled ← decode args[1]!
      return { unfoldCounter, enabled }
    else
      throw s!"Diagnostics expects 2 args, got {args.size}"

instance : Serializable Lean.Options where
  encode := encode (α := Lean.KVMap)
  decode := decode (α := Lean.KVMap)

instance : Serializable Lean.ModuleData where
  encode md := do
    let encodedIsModule ← encode md.isModule
    let encodedImports ← encode md.imports
    let encodedConstNames ← encode md.constNames
    let encodedConstants ← encode md.constants
    let encodedExtraConstNames ← encode md.extraConstNames
    return .compound "ModuleData" #[encodedIsModule, encodedImports, encodedConstNames, encodedConstants, encodedExtraConstNames]
  decode sv := do
    let args ← decodeCompound "ModuleData" sv
    if args.size = 5 then do
      let isModule ← decode args[0]!
      let imports ← decode args[1]!
      let constNames ← decode args[2]!
      let constants ← decode args[3]!
      let extraConstNames ← decode args[4]!
      return {
        isModule,
        imports,
        constNames,
        constants,
        extraConstNames,
        entries := Array.empty  -- Not serialized
      }
    else
      throw s!"ModuleData expects 5 args, got {args.size}"

instance : Serializable Lean.CompactedRegion where
  encode r := return .nat r.toNat
  decode := fun
    | .nat n => return (USize.ofNat n)
    | other => throw s!"Expected CompactedRegion, got {repr other}"

deriving instance LeanSerde.Serializable for Lean.EnvironmentHeader

-- Serialize partial Environment
namespace EnvironmentBuilder

private def ensureSearchPath : IO Unit := do
  -- This is kinda hacky, reconsider
  let paths ← searchPathRef.get
  if paths.isEmpty then
    Lean.initSearchPath (← Lean.findSysroot)

def fromImports (imports : List String) (trustLevel : UInt32 := 0) : IO Lean.Environment := do
  ensureSearchPath
  let imports := imports.map (·.toName) |>.map ({ module := · })
  importModules imports.toArray {} trustLevel

def empty : IO Lean.Environment := do
  ensureSearchPath
  mkEmptyEnvironment

def minimal : IO Lean.Environment := do
  ensureSearchPath
  return (← fromImports ["Init"])

def std : IO Lean.Environment := do
  ensureSearchPath
  return (← fromImports ["Std", "Lean", "Init"])

end EnvironmentBuilder

structure EnvironmentConfig where
  imports : Array Import
  constants : List (Name × ConstantInfo)
  deriving LeanSerde.Serializable

instance : Serializable Lean.Environment where
  encode env := encode {
    imports := env.header.imports
    constants := env.constants.map₂.toList : EnvironmentConfig
  }
  decode sv := do
    EnvironmentBuilder.ensureSearchPath
    let senv : EnvironmentConfig ← decode sv
    let baseEnv ← importModules senv.imports {} 0 #[] false true
    baseEnv.replay (Std.HashMap.ofList senv.constants)

-- Towards InfoTree
-- StateM & Dynamic: Encode type information only
instance {σ α} [Inhabited α] : Serializable (StateM σ α) where
  encode _ := return .compound "StateM.phantom" #[]
  decode _ := return (fun s => (default, s))

deriving instance TypeName for String

instance : Serializable Dynamic where
  encode _ := return .compound "Dynamic.phantom" #[]
  decode _ := return Dynamic.mk "phantom"

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

partial def encodeInfoTree (it : Lean.Elab.InfoTree) : IO SerialValue :=
  match it with
  | .context i t => do
    let encodedI ← encode i
    let encodedT ← encodeInfoTree t
    return .compound "InfoTree.context" #[encodedI, encodedT]
  | .node i children => do
    let encodedI ← encode i
    let encodedChildren ← children.toArray.mapM encodeInfoTree
    return .compound "InfoTree.node" #[encodedI, .compound "Array" encodedChildren]
  | .hole mvarId => do
    let encodedMvarId ← encode mvarId
    return .compound "InfoTree.hole" #[encodedMvarId]

partial def decodeInfoTree (sv : SerialValue) : DecodeM Lean.Elab.InfoTree := do
  match sv with
  | .compound "InfoTree.context" #[i, t] =>
    let i ← decode i
    let t ← decodeInfoTree t
    return (.context i t)
  | .compound "InfoTree.node" #[i, .compound "Array" childrenSvs] =>
    let i ← decode i
    let childrenDecoded ← childrenSvs.mapM decodeInfoTree
    let childrenPersistent := childrenDecoded.foldl (fun acc child => acc.push child) Lean.PersistentArray.empty
    return (.node i childrenPersistent)
  | .compound "InfoTree.hole" #[mvarId] =>
    let mvarId ← decode mvarId
    return (.hole mvarId)
  | _ => throw s!"Expected InfoTree compound, got {repr sv}"

instance : Serializable Lean.Elab.InfoTree where
  encode := encodeInfoTree
  decode := decodeInfoTree

-- Various
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
    return ⟨syn⟩

instance {ks : SyntaxNodeKinds} : Serializable (TSyntaxArray ks) :=
  inferInstanceAs (Serializable (Array (TSyntax ks)))

deriving instance LeanSerde.Serializable for Lean.Widget.UserWidgetDefinition

end LeanSerde
