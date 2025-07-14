import Lean.Data.Json

namespace LeanSerial

inductive SerializationFormat where
  | ByteArray | Json | String
  deriving Repr, BEq, Inhabited

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
    .mkObj [("type", .str name), ("args", .arr (children.map toJson))]

partial def fromJson : Lean.Json → Except String SerialValue
  | Lean.Json.str s => pure (SerialValue.str s)
  | Lean.Json.num n =>
    match n.mantissa, n.exponent with
    | m, 0 => if m >= 0 then pure (SerialValue.nat m.natAbs) else throw s!"Expected natural number, got negative {m}"
    | _, _ => throw s!"Expected integer, got decimal {n}"
  | Lean.Json.bool b => pure (SerialValue.bool b)
  | Lean.Json.obj o => do
    let fields := o.fold (fun acc k v => (k, v) :: acc) []

    let typeName ← match fields.find? (fun (k, _) => k == "type") with
      | some (_, Lean.Json.str s) => pure s
      | some (_, _) => throw "Expected string value for 'type' field"
      | none => throw "Missing 'type' field in compound object"

    let argsJson ← match fields.find? (fun (k, _) => k == "args") with
      | some (_, argsJson) => pure argsJson
      | none => throw "Missing 'args' field in compound object"

    let args ← match argsJson with
      | Lean.Json.arr arr => arr.mapM fromJson
      | _ => throw "Expected JSON array"
    pure (SerialValue.compound typeName args)
  | _ => throw "Invalid JSON structure for SerialValue."

instance : Lean.ToJson SerialValue := ⟨toJson⟩
instance : Lean.FromJson SerialValue := ⟨fromJson⟩

end SerialValue

class SerializableFormat (α : Type) where
  serializeValue : SerialValue → α
  deserializeValue : α → Except String SerialValue

instance : SerializableFormat ByteArray where
  serializeValue sv := (Lean.toJson sv).compress.toUTF8
  deserializeValue bytes := do
    let s ← match String.fromUTF8? bytes with
      | some str => pure str
      | none     => throw "Invalid UTF-8 in serialized data"
    let json ← Lean.Json.parse s
    Lean.fromJson? json

instance : SerializableFormat Lean.Json where
  serializeValue sv := Lean.toJson sv
  deserializeValue json := Lean.fromJson? json

instance : SerializableFormat String where
  serializeValue sv := (Lean.toJson sv).pretty
  deserializeValue str := do
    let json ← Lean.Json.parse str
    Lean.fromJson? json


end LeanSerial
