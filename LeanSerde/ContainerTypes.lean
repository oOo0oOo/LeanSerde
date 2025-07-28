import LeanSerde.Core
import LeanSerde.PrimitiveTypes
import LeanSerde.Derive

namespace LeanSerde

-- List
instance [Serializable α] : Serializable (List α) where
  encode lst := .compound "List.cons" (lst.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "List.cons" sv
    let elems ← args.mapM decode
    .ok elems.toList

-- Array
instance [Serializable α] : Serializable (Array α) where
  encode arr := .compound "Array" (arr.map encode)
  decode sv := do
    let args ← decodeCompound "Array" sv
    args.mapM decode

-- Option
instance [Serializable α] : Serializable (Option α) where
  encode opt := match opt with
    | .none => .compound "Option.none" #[]
    | .some x => .compound "Option.some" #[encode x]
  decode sv := match sv with
    | .compound "Option.none" args =>
      if args.size == 0 then .ok .none else .error "Invalid Option.none format"
    | .compound "Option.some" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        .ok (.some val)
      else .error "Invalid Option.some format"
    | _ => .error "Expected Option constructor"

-- Prod
instance [Serializable α] [Serializable β] : Serializable (α × β) where
  encode prod := .compound "Prod" #[encode prod.1, encode prod.2]
  decode sv := do
    let args ← decodeCompound "Prod" sv
    if args.size == 2 then do
      let fst ← decode args[0]!
      let snd ← decode args[1]!
      .ok (fst, snd)
    else
      .error "Invalid Prod format"

-- Sum
instance [Serializable α] [Serializable β] : Serializable (α ⊕ β) where
  encode sum := match sum with
    | .inl x => .compound "Sum.inl" #[encode x]
    | .inr x => .compound "Sum.inr" #[encode x]
  decode sv := match sv with
    | .compound "Sum.inl" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        .ok (.inl val)
      else .error "Invalid Sum.inl format"
    | .compound "Sum.inr" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        .ok (.inr val)
      else .error "Invalid Sum.inr format"
    | _ => .error "Expected Sum constructor"

-- Except
instance [Serializable α] [Serializable β] : Serializable (Except α β) where
  encode exc := match exc with
    | .error x => .compound "Except.error" #[encode x]
    | .ok x => .compound "Except.ok" #[encode x]
  decode sv := match sv with
    | .compound "Except.error" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        .ok (.error val)
      else .error "Invalid Except.error format"
    | .compound "Except.ok" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        .ok (.ok val)
      else .error "Invalid Except.ok format"
    | _ => .error "Expected Except constructor"

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

end LeanSerde
