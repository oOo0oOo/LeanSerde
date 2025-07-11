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

--== Primitive Instances (Direct mapping, no type witnesses) ==--

instance : Serializable String where
  encode s := .str s
  decode
    | .str s => .ok s
    | other => .error s!"Expected String, got {repr other}"

instance : Serializable Nat where
  encode n := .nat n
  decode
    | .nat n => .ok n
    | other => .error s!"Expected Nat, got {repr other}"

instance : Serializable Bool where
  encode b := .bool b
  decode
    | .bool b => .ok b
    | other => .error s!"Expected Bool, got {repr other}"

-- For Int, we need a compound since SerialValue doesn't have int primitive
instance : Serializable Int where
  encode i :=
    if i >= 0 then
      .compound "Int.pos" #[.nat i.toNat]
    else
      .compound "Int.neg" #[.nat (-i).toNat]
  decode sv := do
    match sv with
    | .compound "Int.pos" #[.nat n] => .ok (Int.ofNat n)
    | .compound "Int.neg" #[.nat n] => .ok (-(Int.ofNat n))
    | .compound "Int.pos" args => .error s!"Int.pos expects 1 arg, got {args.size}"
    | .compound "Int.neg" args => .error s!"Int.neg expects 1 arg, got {args.size}"
    | other => .error s!"Expected Int compound, got {repr other}"

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
