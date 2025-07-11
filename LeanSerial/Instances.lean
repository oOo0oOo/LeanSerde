import LeanSerial.Derive

open Lean.Serialization

--== Basic Type Instances ==--

instance : Serialize String := {}
instance : Serialize Nat := {}
instance : Serialize Bool := {}
instance : Serialize Int := {}

--== Container Type Instances ==--

instance [Serialize α] : Serialize (Option α) := {}
instance [Serialize α] : Serialize (List α) := {}
instance [Serialize α] : Serialize (Array α) := {}
