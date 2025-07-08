import LeanSerial

structure MyData where
  name : String
  value : Nat
  deriving Serialize, Repr

#eval do
  let data : MyData := { name := "hello", value := 42 }
  let bytes ← LeanSerial.serialize data
  match bytes with
  | .ok bytes =>
    IO.println s!"Serialized to: {bytes}"
    let result ← LeanSerial.deserialize (α := MyData) bytes
    match result with
    | .ok data' => IO.println s!"Deserialized: {repr data'}"
    | .error e => IO.println s!"Error: {e}"
  | .error e => IO.println s!"Error: {e}"

def main: IO Unit := do
  IO.println "LeanSerial is ready to use!"
