import LeanSerde.Core
import LeanSerde.PrimitiveTypes
import LeanSerde.Derive

namespace LeanSerde

-- List
instance [Serializable α] : Serializable (List α) where
  encode lst := do
    let encodedElems ← lst.mapM encode
    return .compound "List.cons" encodedElems.toArray
  decode sv := do
    let args ← decodeCompound "List.cons" sv
    let elems ← args.mapM decode
    return elems.toList

-- Array
instance [Serializable α] : Serializable (Array α) where
  encode arr := do
    let encodedElems ← arr.mapM encode
    return .compound "Array" encodedElems
  decode sv := do
    let args ← decodeCompound "Array" sv
    args.mapM decode

-- Option
instance [Serializable α] : Serializable (Option α) where
  encode opt := match opt with
    | .none => return .compound "Option.none" #[]
    | .some x => do
      let encodedX ← encode x
      return .compound "Option.some" #[encodedX]
  decode sv := match sv with
    | .compound "Option.none" args =>
      if args.size == 0 then return .none else throw "Invalid Option.none format"
    | .compound "Option.some" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        return .some val
      else throw "Invalid Option.some format"
    | _ => throw "Expected Option constructor"

-- Prod
instance [Serializable α] [Serializable β] : Serializable (α × β) where
  encode prod := do
    let encodedFst ← encode prod.1
    let encodedSnd ← encode prod.2
    return .compound "Prod" #[encodedFst, encodedSnd]
  decode sv := do
    let args ← decodeCompound "Prod" sv
    if args.size == 2 then do
      let fst ← decode args[0]!
      let snd ← decode args[1]!
      return (fst, snd)
    else
      throw "Invalid Prod format"

-- Sum
instance [Serializable α] [Serializable β] : Serializable (α ⊕ β) where
  encode sum := match sum with
    | .inl x => do
      let encodedX ← encode x
      return .compound "Sum.inl" #[encodedX]
    | .inr x => do
      let encodedX ← encode x
      return .compound "Sum.inr" #[encodedX]
  decode sv := match sv with
    | .compound "Sum.inl" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        return .inl val
      else throw "Invalid Sum.inl format"
    | .compound "Sum.inr" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        return .inr val
      else throw "Invalid Sum.inr format"
    | _ => throw "Expected Sum constructor"

-- Except
instance [Serializable α] [Serializable β] : Serializable (Except α β) where
  encode exc := match exc with
    | .error x => do
      let encodedX ← encode x
      return .compound "Except.error" #[encodedX]
    | .ok x => do
      let encodedX ← encode x
      return .compound "Except.ok" #[encodedX]
  decode sv := match sv with
    | .compound "Except.error" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        return (.error val)
      else throw "Invalid Except.error format"
    | .compound "Except.ok" args =>
      if args.size == 1 then do
        let val ← decode args[0]!
        return (.ok val)
      else throw "Invalid Except.ok format"
    | _ => throw "Expected Except constructor"

-- Subarray
instance [Serializable α] : Serializable (Subarray α) where
  encode xs := do
    let encodedElems ← xs.toArray.mapM encode
    return .compound "Subarray" encodedElems
  decode sv := do
    let args ← decodeCompound "Subarray" sv
    let arr ← args.mapM decode
    return ⟨arr, 0, arr.size, Nat.zero_le _, Nat.le_refl _⟩

-- ByteArray
instance : Serializable ByteArray where
  encode ba := do
    let encodedBytes := ba.data.map (fun b => .nat b.toNat)
    return .compound "ByteArray" encodedBytes
  decode sv := do
    let args ← decodeCompound "ByteArray" sv
    let bytes ← args.mapM (fun v => match v with
      | .nat n =>
        if n ≤ 255 then
          return UInt8.ofNat n
        else
          throw s!"Byte value {n} out of range"
      | _ => throw s!"Expected Nat, got {repr v}")
    return ⟨bytes⟩

-- Fin
instance {n : Nat} : Serializable (Fin n) where
  encode f := return .compound "Fin" #[.nat n, .nat f.val]
  decode sv := do
    match sv with
    | .compound "Fin" #[.nat bound, .nat val] =>
      if bound ≠ n then
        throw s!"Fin bound mismatch: expected {n}, got {bound}"
      else if h : val < n then
        return ⟨val, h⟩
      else
        throw s!"Fin value {val} not less than bound {n}"
    | .compound "Fin" args =>
      throw s!"Fin expects 2 args, got {args.size}"
    | other =>
      throw s!"Expected Fin compound, got {repr other}"

-- Thunk (Unit → α)
instance {α : Type} [Serializable α] : Serializable (Unit → α) where
  encode f := do
    let encodedA ← encode (f ())
    return .compound "Thunk" #[encodedA]
  decode sv := do
    match sv with
    | .compound "Thunk" #[av] =>
      let a ← decode av
      return (fun _ => a)
    | .compound "Thunk" args =>
      throw s!"Thunk expects 1 arg, got {args.size}"
    | other =>
      throw s!"Expected Thunk compound, got {repr other}"

end LeanSerde
