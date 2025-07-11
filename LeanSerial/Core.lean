import Lean

namespace LeanSerial

/--
Simplified serialization format with unified structure.
Primitives have no type witnesses for efficiency.
-/
inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

/-- Convert SerialValue to JSON-like string representation -/
def toString : SerialValue → String
  | str s => s!"\"{s}\""
  | nat n => Nat.repr n
  | bool b => if b then "true" else "false"
  | compound name children =>
  let childrenStr := String.intercalate "," (children.map toString).toList
  s!"\{\"type\":\"{name}\",\"args\":[{childrenStr}]}"

/-- Parse JSON-like string back to SerialValue -/
private def parseCompound (s : String) : Except String SerialValue :=
  -- Simplified parsing for MVP
  .error "Compound parsing not implemented in MVP"

partial def fromString (s : String) : Except String SerialValue :=
  -- Simple parser for MVP - would use proper JSON parser in production
  if s.startsWith "\"" && s.endsWith "\"" then
    .ok (str (s.drop 1 |>.dropRight 1))
  else if s == "true" then
    .ok (bool true)
  else if s == "false" then
    .ok (bool false)
  else if let some n := s.toNat? then
    .ok (nat n)
  else if s.startsWith "{\"type\":" then
    parseCompound s
  else
    .error s!"Invalid SerialValue format: {s}"

end SerialValue

def serializeValue (sv : SerialValue) : ByteArray :=
  sv.toString.toUTF8

def deserializeValue (bytes : ByteArray) : Except String SerialValue :=
  match String.fromUTF8? bytes with
  | none => .error "Invalid UTF-8 in serialized data"
  | some s => SerialValue.fromString s

end LeanSerial
