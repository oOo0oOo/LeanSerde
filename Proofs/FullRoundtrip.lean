import LeanSerial

open LeanSerial

@[simp] theorem json_roundtrip (gd : GraphData) :
  SerializableFormat.deserializeValue (SerializableFormat.serializeValue gd : Lean.Json) = Except.ok gd := by
  sorry

@[simp] theorem withCycleCheck_roundtrip {α : Type} [Serializable α] (x : α) :
  StateT.run (withCycleCheck x) EncodeState.empty = (encode x, EncodeState.empty) := by
  sorry

@[simp] theorem decodeGraph_roundtrip {α : Type} [Serializable α] (x : α) :
  decodeGraph ⟨encode x, #[]⟩ = Except.ok x := by
  unfold decodeGraph
  sorry

@[simp] theorem bind_ok {α β : Type} (a : α) (f : α → Except String β) :
  (Except.ok a >>= f) = f a := rfl

@[simp] theorem EncodeState_empty_objects : EncodeState.empty.objects = #[] := rfl

theorem roundtrip_theorem {α : Type} [Serializable α] (x : α) :
  deserialize (serialize x : Lean.Json) = Except.ok x := by
    simp only [serialize, deserialize, encodeGraph,
      withCycleCheck_roundtrip, json_roundtrip,
      bind_ok, EncodeState_empty_objects, decodeGraph_roundtrip]

def String_roundtrip := @roundtrip_theorem String
def Nat_roundtrip := @roundtrip_theorem Nat
def Bool_roundtrip := @roundtrip_theorem Bool
def Unit_roundtrip := @roundtrip_theorem Unit
