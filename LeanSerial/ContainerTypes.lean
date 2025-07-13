import LeanSerial.PrimitiveTypes

namespace LeanSerial

-- Product Types
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

-- Option
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

-- Except
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

-- List
instance [Serializable α] : Serializable (List α) where
  encode xs := .compound "List" (xs.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "List" sv
    let xs ← args.mapM decode |>.mapError (·)
    .ok xs.toList

-- Array
instance [Serializable α] : Serializable (Array α) where
  encode xs := .compound "Array" (xs.map encode)
  decode sv := do
    let args ← decodeCompound "Array" sv
    args.mapM decode |>.mapError (·)

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
