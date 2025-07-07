import LeanSerial

structure MyData where
  name : String
  value : Nat
  deriving Repr

derive_ToExpr MyData
derive_FromExpr MyData

-- Alternative approach: run serialization within CommandElabM
#eval do
  let data : MyData := { name := "hello", value := 42 }

  -- Use the core functions directly
  let bytes ← LeanSerial.serializeInCore data
  IO.println s!"Serialized to: {bytes}"

  let data' : MyData ← LeanSerial.deserializeInCore bytes
  IO.println s!"Deserialized: {repr data'}"

  if data.name == data'.name && data.value == data'.value then
    IO.println "✓ Serialization and deserialization successful!"
  else
    IO.println "✗ Data mismatch!"

def main : IO Unit := do
  IO.println "Running serialization test..."
