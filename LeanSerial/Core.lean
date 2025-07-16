import Lean.Data.Json

namespace LeanSerial

inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

def toJson : SerialValue → Lean.Json
  | .str s => .str s
  | .nat n => .num n
  | .bool b => .bool b
  | .compound name children =>
    .arr #[.str name, .arr (children.map toJson)]

partial def fromJson : Lean.Json → Except String SerialValue
  | .str s => .ok (.str s)
  | .num n =>
    if n.exponent == 0 && n.mantissa >= 0 then
      .ok (.nat n.mantissa.natAbs)
    else
      .error s!"Expected natural number, got {n}"
  | .bool b => .ok (.bool b)
  | .arr #[.str name, .arr children] => do
    let args ← children.mapM fromJson
    .ok (.compound name args)
  | .arr arr => .error s!"Expected [name, args], got array of size {arr.size}"
  | _ => .error "Invalid SerialValue JSON"

instance : Lean.ToJson SerialValue := ⟨toJson⟩
instance : Lean.FromJson SerialValue := ⟨fromJson⟩

end SerialValue

class SerializableFormat (α : Type) where
  serializeValue : SerialValue → α
  deserializeValue : α → Except String SerialValue

instance : SerializableFormat Lean.Json where
  serializeValue sv := Lean.toJson sv
  deserializeValue json := Lean.fromJson? json

instance : SerializableFormat String where
  serializeValue sv := (Lean.toJson sv).pretty
  deserializeValue str := do
    let json ← Lean.Json.parse str
    Lean.fromJson? json

-- Serialized class
abbrev DecodeM := Except String

class Serializable (α : Type) where
  encode : α → SerialValue
  decode : SerialValue → DecodeM α

export Serializable (encode decode)

def decodeCompound (expectedName : String) (sv : SerialValue) : DecodeM (Array SerialValue) :=
  match sv with
  | .compound name args =>
    if name == expectedName then .ok args
    else .error s!"Expected {expectedName}, got {name}"
  | other => .error s!"Expected compound {expectedName}, got {repr other}"

end LeanSerial
