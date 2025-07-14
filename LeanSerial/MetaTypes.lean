import Lean
import Lean.Data.KVMap
import LeanSerial.Derive
import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes

open Lean

namespace LeanSerial

-- Towards Level
deriving instance Serializable for Lean.Name
deriving instance Serializable for Lean.LevelMVarId
deriving instance Serializable for Lean.Level

-- Towards Syntax
deriving instance Serializable for Lean.Syntax.Preresolved

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

deriving instance Serializable for Lean.Syntax

-- Towards Expr
deriving instance Serializable for Lean.FVarId
deriving instance Serializable for Lean.MVarId
deriving instance Serializable for Lean.BinderInfo
deriving instance Serializable for Lean.Literal
deriving instance Serializable for Lean.DataValue

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

deriving instance Serializable for Lean.Expr

end LeanSerial
