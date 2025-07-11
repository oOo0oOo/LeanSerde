import Lean
import Std.Data.HashMap
import Std.Data.HashSet
import Std.Data.TreeMap
import Std.Data.TreeSet

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

-- Primitive Types: String, Char, FilePath
instance : Serializable String where
  encode s := .str s
  decode
    | .str s => .ok s
    | other => .error s!"Expected String, got {repr other}"

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

-- Sum Types
instance {α β : Type} [Serializable α] [Serializable β] : Serializable (Sum α β) where
  encode
    | Sum.inl a => .compound "Sum.inl" #[encode a]
    | Sum.inr b => .compound "Sum.inr" #[encode b]
  decode sv := do
    match sv with
    | .compound "Sum.inl" #[av] =>
      let a ← decode av
      .ok (Sum.inl a)
    | .compound "Sum.inr" #[bv] =>
      let b ← decode bv
      .ok (Sum.inr b)
    | .compound "Sum.inl" args =>
      .error s!"Sum.inl expects 1 arg, got {args.size}"
    | .compound "Sum.inr" args =>
      .error s!"Sum.inr expects 1 arg, got {args.size}"
    | other =>
      .error s!"Expected Sum compound, got {repr other}"


-- Container Types
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

instance [Serializable α] : Serializable (Subarray α) where
  encode xs := .compound "Subarray" (xs.toArray.map encode)
  decode sv := do
    let args ← decodeCompound "Subarray" sv
    let arr ← args.mapM decode |>.mapError (·)
    .ok ⟨arr, 0, arr.size, Nat.zero_le _, Nat.le_refl _⟩

instance {n : Nat} : Serializable (Fin n) where
  encode f := .compound "Fin" #[.nat n, .nat f.val]
  decode sv := do
    match sv with
    | .compound "Fin" #[.nat bound, .nat val] =>
      if bound ≠ n then
        .error s!"Fin bound mismatch: expected {n}, got {bound}"
      else if h : val < n then
        .ok ⟨val, h⟩
      else
        .error s!"Fin value {val} not less than bound {n}"
    | .compound "Fin" args =>
      .error s!"Fin expects 2 args, got {args.size}"
    | other =>
      .error s!"Expected Fin compound, got {repr other}"

instance : Serializable ByteArray where
  encode ba := .compound "ByteArray" (ba.data.map (fun b => .nat b.toNat))
  decode sv := do
    let args ← decodeCompound "ByteArray" sv
    let bytes ← args.mapM (fun v => match v with
      | .nat n =>
        if n ≤ 255 then
          .ok (UInt8.ofNat n)
        else
          .error s!"Byte value {n} out of range"
      | _ => .error s!"Expected Nat, got {repr v}")
    .ok ⟨bytes⟩


-- HashMap, HashSet
instance [Serializable k] [Serializable v] [BEq k] [Hashable k] : Serializable (Std.HashMap k v) where
  encode m := .compound "HashMap" ((m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v])).toArray)
  decode sv := do
    let args ← decodeCompound "HashMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (Std.HashMap.ofList entries.toList)

instance [Serializable k] [BEq k] [Hashable k] : Serializable (Std.HashSet k) where
  encode s := .compound "HashSet" (s.toList.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "HashSet" sv
    let elems ← args.mapM decode |>.mapError (·)
    .ok (Std.HashSet.ofList elems.toList)


-- Functional types
instance : Serializable Unit where
  encode _ := .compound "Unit" #[]
  decode
    | .compound "Unit" #[] => .ok ()
    | other => .error s!"Expected Unit, got {repr other}"

instance {e : Type} [Serializable e] {a : Type} [Serializable a] : Serializable (Except e a) where
  encode
    | .ok a => .compound "Except.ok" #[encode a]
    | .error e => .compound "Except.error" #[encode e]
  decode sv := do
    match sv with
    | .compound "Except.ok" #[av] =>
      let a ← decode av
      .ok (.ok a)
    | .compound "Except.error" #[ev] =>
      let e ← decode ev
      .ok (.error e)
    | .compound "Except.ok" args =>
      .error s!"Except.ok expects 1 arg, got {args.size}"
    | .compound "Except.error" args =>
      .error s!"Except.error expects 1 arg, got {args.size}"
    | other =>
      .error s!"Expected Except compound, got {repr other}"

instance {α : Type} [Serializable α] : Serializable (Unit → α) where
  encode f := .compound "Thunk" #[encode (f ())]
  decode sv := do
    match sv with
    | .compound "Thunk" #[av] =>
      let a ← decode av
      .ok (fun _ => a)
    | .compound "Thunk" args =>
      .error s!"Thunk expects 1 arg, got {args.size}"
    | other =>
      .error s!"Expected Thunk compound, got {repr other}"

end LeanSerial
