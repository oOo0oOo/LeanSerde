import LeanSerial

open LeanSerial

-- Define a class for types that have perfect encode/decode roundtrips
class HasEncodeDecodeRoundtrip (α : Type) [Serializable α] : Prop where
  roundtrip : ∀ (x : α), decode (encode x) = Except.ok x

instance : HasEncodeDecodeRoundtrip Nat where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip UInt8 where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip UInt16 where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip UInt32 where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip UInt64 where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip String where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip String.Pos where
  roundtrip := fun _ => rfl

instance : HasEncodeDecodeRoundtrip Char where
  roundtrip := fun _ => rfl

instance : HasEncodeDecodeRoundtrip System.FilePath where
  roundtrip := fun _ => rfl

instance : HasEncodeDecodeRoundtrip Bool where
  roundtrip x := by simp [encode, decode]

instance : HasEncodeDecodeRoundtrip Unit where
  roundtrip := fun _ => rfl
