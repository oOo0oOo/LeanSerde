import LeanSerial

-- Custom types by deriving `LeanSerial.Serializable`
structure FileNode where
  name : String
  children : Array FileNode  -- Recursive!
  validated : Option Bool
  deriving LeanSerial.Serializable, BEq

def main : IO Unit := do
  -- Create a recursive file structure
  let file1 : FileNode := { name := "file1.txt", children := #[], validated := some false }
  let file2 : FileNode := { name := "file2.bin", children := #[file1], validated := true }
  let file3 : FileNode := { name := "file3.txt", children := #[file1, file1], validated := some true }

  -- Serialize to different formats
  let bytes : ByteArray := LeanSerial.serialize file1 -- Binary format (CBOR)
  let _json : Lean.Json := LeanSerial.serialize file2
  let _string : String := LeanSerial.serialize file3

  match LeanSerial.deserialize bytes with
  | .ok (node: FileNode) =>
    if node == file1 then
      IO.println "Roundtrip successful!"
    else
      IO.println "Roundtrip failed!"
  | .error msg => IO.println s!"Error: {msg}"

  -- Serialize directly to/from file
  LeanSerial.serializeToFile file3 "serialized.cbor"
  LeanSerial.serializeToJsonFile file3 "serialized.json"

  match (← LeanSerial.deserializeFromFile "serialized.cbor") with
  | .ok node =>
    if node == file3 then
      IO.println "File roundtrip successful!"
  | .error msg => IO.println s!"Error loading file: {msg}"

  -- Supports variety of types
  let _ : ByteArray := LeanSerial.serialize [3, 1, 4]
  let _ : ByteArray := LeanSerial.serialize #[1.1, 2.2, 3.2]
  let _ : ByteArray := LeanSerial.serialize (Sum.inl 42 : Sum Nat String)
  let _ : ByteArray := LeanSerial.serialize (.ok "success" : Except String String)
  let _ : ByteArray := LeanSerial.serialize (("key", 123), ("value", 456))
  let _ : ByteArray := LeanSerial.serialize [true, false, true]
  let _ : ByteArray := LeanSerial.serialize [[1, 2], [3, 4], []]
  let _ : ByteArray := LeanSerial.serialize (System.FilePath.mk "/tmp/test.txt")
  let _ : ByteArray := LeanSerial.serialize (Std.Time.PlainDateTime.ofDaysSinceUNIXEpoch 1000 ⟨0, 0, 0, 0⟩)
  let _ : ByteArray := LeanSerial.serialize (some (some (some 42)))
