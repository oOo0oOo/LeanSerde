import Lean.Data.Json

namespace LeanSerial

inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

def toJsonImpl : SerialValue → Lean.Json
  | str s    => Lean.Json.str s
  | nat n    => Lean.Json.num n
  | bool b   => Lean.Json.bool b
  | compound name children =>
    Lean.Json.mkObj [("type", Lean.Json.str name), ("args", Lean.Json.arr (children.map toJsonImpl))]

mutual

partial def fromJsonArrayImpl : Lean.Json → Except String (Array SerialValue)
  | Lean.Json.arr arr => do
    let values ← arr.mapM fromJsonImpl
    pure values
  | _ => throw "Expected JSON array"

partial def fromJsonImpl : Lean.Json → Except String SerialValue
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

    let args ← fromJsonArrayImpl argsJson
    pure (SerialValue.compound typeName args)
  | _ => throw "Invalid JSON structure for SerialValue."

end

instance : Lean.ToJson SerialValue where
  toJson := toJsonImpl

instance : Lean.ToJson (Array SerialValue) where
  toJson arr := Lean.Json.arr (arr.map toJsonImpl)

instance : Lean.FromJson SerialValue where
  fromJson? := fromJsonImpl

instance : Lean.FromJson (Array SerialValue) where
  fromJson? := fromJsonArrayImpl

end SerialValue

def serializeValue (sv : SerialValue) : ByteArray :=
  (Lean.toJson sv).compress.toUTF8

def deserializeValue (bytes : ByteArray) : Except String SerialValue := do
  let s ← match String.fromUTF8? bytes with
    | some str => pure str
    | none     => throw "Invalid UTF-8 in serialized data"
  let json ← Lean.Json.parse s
  Lean.fromJson? json

end LeanSerial
