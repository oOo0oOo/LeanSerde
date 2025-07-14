import Lean
import Lean.Data.KVMap
import LeanSerial.Derive
import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes

open Lean

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
run_cmd mkSerializableInstance `Lean.Syntax  -- Much faster!!

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

end LeanSerial
