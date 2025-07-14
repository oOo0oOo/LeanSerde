import Lean
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


mutual
  partial def encodeSyntax (syn : Lean.Syntax) : LeanSerial.SerialValue :=
    match syn with
    | .atom info val => .compound "Syntax.atom" #[encode info, encode val]
    | .ident info rawVal val preresolved => .compound "Syntax.ident" #[encode info, encode rawVal, encode val, encode preresolved]
    | .node info kind args => .compound "Syntax.node" #[encode info, encode kind, encodeArraySyntax args]
    | .missing => .compound "Syntax.missing" #[]

  partial def decodeSyntax (sv : LeanSerial.SerialValue) : Except String Lean.Syntax := do
    match sv with
    | .compound "Syntax.atom" #[info, val] => do
      let info ← decode info
      let val ← decode val
      .ok (Lean.Syntax.atom info val)
    | .compound "Syntax.ident" #[info, rawVal, val, preresolved] => do
      let info ← decode info
      let rawVal ← decode rawVal
      let val ← decode val
      let preresolved ← decode preresolved
      .ok (Lean.Syntax.ident info rawVal val preresolved)
    | .compound "Syntax.node" #[info, kind, args] => do
      let info ← decode info
      let kind ← decode kind
      let args ← decodeArraySyntax args
      .ok (Lean.Syntax.node info kind args)
    | .compound "Syntax.missing" #[] =>
      .ok Lean.Syntax.missing
    | .compound name args =>
      .error s!"Unknown Syntax constructor: {name} with {args.size} args"
    | other =>
      .error s!"Expected Syntax compound, got {repr other}"

  partial def encodeArraySyntax (arr : Array Lean.Syntax) : LeanSerial.SerialValue :=
    .compound "Array" (arr.map encodeSyntax)

  partial def decodeArraySyntax (sv : LeanSerial.SerialValue) : Except String (Array Lean.Syntax) := do
    let args ← decodeCompound "Array" sv
    args.mapM decodeSyntax |>.mapError (·)
end

instance : Serializable Lean.Syntax where
  encode := encodeSyntax
  decode := decodeSyntax

-- run_cmd mkSerializableInstance `Lean.Literal
-- run_cmd mkSerializableInstance `Lean.BinderInfo
-- run_cmd mkSerializableInstance `Lean.FVarId
-- run_cmd mkSerializableInstance `Lean.MVarId



-- run_cmd mkSerializableInstance `Lean.DataValue
-- run_cmd mkSerializableInstance `Lean.KVMap
-- run_cmd mkSerializableInstance `Lean.Expr

end LeanSerial
