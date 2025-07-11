import Lean
import LeanSerial.Core

namespace LeanSerial

/-- Monadic error handling for cleaner decode operations -/
abbrev DecodeM := Except String

/--
Simplified serialization typeclass with monadic error handling.
Primitives map directly to SerialValue variants.
-/
class Serializable (α : Type) where
  encode : α → SerialValue
  decode : SerialValue → DecodeM α

export Serializable (encode decode)

-- Helper for compound decoding
def decodeCompound (expectedName : String) (sv : SerialValue) : DecodeM (Array SerialValue) :=
  match sv with
  | .compound name args =>
    if name == expectedName then .ok args
    else .error s!"Expected {expectedName}, got {name}"
  | other => .error s!"Expected compound {expectedName}, got {repr other}"

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

-- Primitive Types: String, Char and Tuples
instance : Serializable String where
  encode s := .str s
  decode
    | .str s => .ok s
    | other => .error s!"Expected String, got {repr other}"

instance : Serializable Char where
  encode c := .str (String.mk [c])
  decode
    | .str s =>
      if s.length == 1 then
        .ok (s.get 0)
      else
        .error s!"Expected Char, got string of length {s.length}"
    | other => .error s!"Expected Char, got {repr other}"

instance {α β : Type} [Serializable α] [Serializable β] : Serializable (α × β) where
  encode
    | ⟨a, b⟩ => .compound "Prod" #[encode a, encode b]
  decode sv := do
    let args ← decodeCompound "Prod" sv
    if args.size == 2 then
      let a ← decode args[0]!
      let b ← decode args[1]!
      .ok ⟨a, b⟩
    else
      .error s!"Prod expects 2 args, got {args.size}"

instance : Serializable Bool where
  encode b := .bool b
  decode
    | .bool b => .ok b
    | other => .error s!"Expected Bool, got {repr other}"

--== Container Instances ==--

instance [Serializable α] : Serializable (Option α) where
  encode
    | none => .compound "Option.none" #[]
    | some a => .compound "Option.some" #[encode a]
  decode sv := do
    match sv with
    | .compound "Option.none" #[] => .ok none
    | .compound "Option.some" #[av] => do
      let a ← decode av
      .ok (some a)
    | .compound "Option.some" args => .error s!"Option.some expects 1 arg, got {args.size}"
    | other => .error s!"Expected Option compound, got {repr other}"

instance [Serializable α] : Serializable (List α) where
  encode xs := .compound "List" (xs.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "List" sv
    let xs ← args.mapM decode |>.mapError (·)
    .ok xs.toList

instance [Serializable α] : Serializable (Array α) where
  encode xs := .compound "Array" (xs.map encode)
  decode sv := do
    let args ← decodeCompound "Array" sv
    args.mapM decode |>.mapError (·)

end LeanSerial
