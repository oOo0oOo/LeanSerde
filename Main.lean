import LeanSerial

structure MyData where
  name : String
  value : Nat
  deriving Repr, Serialize


#eval do
  let data : MyData := { name := "hello", value := 42 }
  let bytes ← LeanSerial.serialize data
  IO.println s!"Serialized to: {bytes}"
  let data': MyData ← LeanSerial.deserialize bytes
  IO.println s!"Deserialized: {repr data'}"


def main : IO Unit := do
  IO.println "Running serialization test..."
