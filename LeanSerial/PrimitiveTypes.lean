import LeanSerial.Core

namespace LeanSerial

-- Primitive Types: Numeric
instance : Serializable Nat where
  encode n := .nat n
  decode
    | .nat n => .ok n
    | other => .error s!"Expected Nat, got {repr other}"

instance : Serializable UInt8 where
  encode n := .nat n.toNat
  decode
    | .nat n => .ok (UInt8.ofNat n)
    | other => .error s!"Expected UInt8, got {repr other}"

instance : Serializable UInt16 where
  encode n := .nat n.toNat
  decode
    | .nat n => .ok (UInt16.ofNat n)
    | other => .error s!"Expected UInt16, got {repr other}"

instance : Serializable UInt32 where
  encode n := .nat n.toNat
  decode
    | .nat n => .ok (UInt32.ofNat n)
    | other => .error s!"Expected UInt32, got {repr other}"

instance : Serializable UInt64 where
  encode n := .nat n.toNat
  decode
    | .nat n => .ok (UInt64.ofNat n)
    | other => .error s!"Expected UInt64, got {repr other}"

instance : Serializable Float where
  encode f := .compound "Float" #[.nat f.toBits.toNat]
  decode sv := do
    match sv with
    | .compound "Float" #[.nat bits] =>
      if bits < 0 then
        .error s!"Invalid Float bits: {bits}"
      else
        .ok (Float.ofBits bits)
    | .compound "Float" args => .error s!"Float expects 1 arg, got {args.size}"
    | other => .error s!"Expected Float compound, got {repr other}"

-- For Int, we need a compound since SerialValue doesn't have int primitive
class SignedIntLike (α : Type) where
  toInt : α → Int
  ofInt : Int → α
  name : String

instance [inst : SignedIntLike α] : Serializable α where
  encode i :=
    let intVal := inst.toInt i
    let tag := if intVal >= 0 then s!"{inst.name}.pos" else s!"{inst.name}.neg"
    .compound tag #[.nat intVal.natAbs]
  decode sv := do
    let name := inst.name
    match sv with
    | .compound tag #[.nat n] =>
      let sign := if tag == s!"{name}.pos" then 1 else if tag == s!"{name}.neg" then -1 else 0
      if sign == 0 then .error s!"Expected {name} compound"
      else .ok (inst.ofInt (sign * Int.ofNat n))
    | .compound tag args =>
      if tag.startsWith name then
        .error s!"{tag} expects 1 arg, got {args.size}"
      else
        .error s!"Expected {name} compound"
    | _ => .error s!"Expected {name} compound"

instance : SignedIntLike Int where
  toInt := id
  ofInt := id
  name := "Int"

instance : SignedIntLike Int8 where
  toInt := Int8.toInt
  ofInt := Int8.ofInt
  name := "Int8"

instance : SignedIntLike Int16 where
  toInt := Int16.toInt
  ofInt := Int16.ofInt
  name := "Int16"

instance : SignedIntLike Int32 where
  toInt := Int32.toInt
  ofInt := Int32.ofInt
  name := "Int32"

instance : SignedIntLike Int64 where
  toInt := Int64.toInt
  ofInt := Int64.ofInt
  name := "Int64"

-- String, Char, FilePath, Bool
instance : Serializable String where
  encode s := .str s
  decode
    | .str s => .ok s
    | other => .error s!"Expected String, got {repr other}"

instance : LeanSerial.Serializable String.Pos where
  encode pos := .compound "StringPos" #[.nat pos.byteIdx]
  decode sv := do
    match sv with
    | .compound "StringPos" #[.nat idx] => .ok { byteIdx := idx }
    | .compound "StringPos" args => .error s!"StringPos expects 1 arg, got {args.size}"
    | other => .error s!"Expected StringPos compound, got {repr other}"

instance : Serializable Substring where
  encode sub := .compound "Substring" #[encode sub.str, encode sub.startPos, encode sub.stopPos]
  decode sv := do
    match sv with
    | .compound "Substring" #[str, startPos, stopPos] =>
      let str ← decode str
      let startPos ← decode startPos
      let stopPos ← decode stopPos
      if startPos.byteIdx < 0 || stopPos.byteIdx < 0 || stopPos.byteIdx < startPos.byteIdx || stopPos.byteIdx > str.length then
        .error s!"Invalid Substring: start {startPos.byteIdx}, stop {stopPos.byteIdx}, length {str.length}"
      else
        .ok { str := str, startPos := startPos, stopPos := stopPos }
    | .compound "Substring" args => .error s!"Substring expects 3 args, got {args.size}"
    | other => .error s!"Expected Substring compound, got {repr other}"

instance : Serializable Char where
  encode c := .compound "Char" #[.str (String.mk [c])]
  decode
    | .compound "Char" #[.str s] =>
      if s.length == 1 then
        .ok (s.get 0)
      else
        .error s!"Expected Char, got string of length {s.length}"
    | .compound "Char" args => .error s!"Char expects 1 arg, got {args.size}"
    | other => .error s!"Expected Char compound, got {repr other}"

instance : Serializable System.FilePath where
  encode fp := .compound "FilePath" #[.str (fp.toString)]
  decode
    | .compound "FilePath" #[.str s] =>
      .ok (System.FilePath.mk s)
    | .compound "FilePath" args => .error s!"FilePath expects 1 arg, got {args.size}"
    | other => .error s!"Expected FilePath compound, got {repr other}"

instance : Serializable Bool where
  encode b := .bool b
  decode
    | .bool b => .ok b
    | other => .error s!"Expected Bool, got {repr other}"

-- Unit
instance : Serializable Unit where
  encode _ := .compound "Unit" #[]
  decode
    | .compound "Unit" #[] => .ok ()
    | other => .error s!"Expected Unit, got {repr other}"

end LeanSerial
