import LeanSerial.Core
import LeanSerial.PrimitiveTypes
import LeanSerial.Derive

namespace LeanSerial

run_cmd mkSerializableInstance `Prod
run_cmd mkSerializableInstance `Sum
run_cmd mkSerializableInstance `Option
run_cmd mkSerializableInstance `Except
run_cmd mkSerializableInstance `List
run_cmd mkSerializableInstance `Array

-- Subarray
instance [Serializable α] : Serializable (Subarray α) where
  encode xs := .compound "Subarray" (xs.toArray.map encode)
  decode sv := do
    let args ← decodeCompound "Subarray" sv
    let arr ← args.mapM decode |>.mapError (·)
    .ok ⟨arr, 0, arr.size, Nat.zero_le _, Nat.le_refl _⟩

-- Fin
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

-- ByteArray
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

-- Thunk (Unit → α)
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
