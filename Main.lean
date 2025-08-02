import LeanSerde
import LeanSerde.SnapshotTypes

-- Custom types by deriving `LeanSerde.Serializable`
structure FileNode where
  name : String
  children : Array FileNode  -- Recursive!
  validated : Option Bool
  deriving LeanSerde.Serializable, BEq, Inhabited  -- Inhabited is only needed for describeFormat()

def main : IO Unit := do
  -- Create a recursive file structure
  let file1 : FileNode := { name := "file1.txt", children := #[], validated := some false }
  let file2 : FileNode := { name := "file2.bin", children := #[file1], validated := true }
  let file3 : FileNode := { name := "file3.txt", children := #[file1, file1], validated := some true }

  -- Serialize to different formats
  let bytes : ByteArray := ← LeanSerde.serialize file1 -- Binary format (CBOR)
  let _json : Lean.Json := ← LeanSerde.serialize file2
  let _string : String := ← LeanSerde.serialize file3

  match (← LeanSerde.deserialize bytes) with
  | .ok (node: FileNode) =>
    if node == file1 then
      IO.println "Roundtrip successful!"
    else
      IO.println "Roundtrip failed!"
  | .error msg => IO.println s!"Error: {msg}"

  -- Serialize directly to/from file
  LeanSerde.serializeToFile file3 "serialized.cbor"
  LeanSerde.serializeToJsonFile file3 "serialized.json"

  match (← LeanSerde.deserializeFromFile "serialized.cbor") with
  | .ok node =>
    if node == file3 then
      IO.println "File roundtrip successful!"
  | .error msg => IO.println s!"Error loading file: {msg}"

  -- Supports variety of types
  let _ : ByteArray := ← LeanSerde.serialize [3, 1, 4]
  let _ : ByteArray := ← LeanSerde.serialize #[1.1, 2.2, 3.2]
  let _ : ByteArray := ← LeanSerde.serialize (Sum.inl 42 : Sum Nat String)
  let _ : ByteArray := ← LeanSerde.serialize (.ok "success" : Except String String)
  let _ : ByteArray := ← LeanSerde.serialize (("key", 123), ("value", 456))
  let _ : ByteArray := ← LeanSerde.serialize [true, false, true]
  let _ : ByteArray := ← LeanSerde.serialize [[1, 2], [3, 4], []]
  let _ : ByteArray := ← LeanSerde.serialize (System.FilePath.mk "/tmp/test.txt")
  let _ : ByteArray := ← LeanSerde.serialize (Std.Time.PlainDateTime.ofDaysSinceUNIXEpoch 1000 ⟨0, 0, 0, 0⟩)
  let _ : ByteArray := ← LeanSerde.serialize (some (some (some (42 : Nat))))

  -- Describe the serialization format
  match ← LeanSerde.describeFormat FileNode with
  | .ok json => IO.println s!"Format description: {json}"
  | .error msg => IO.println s!"Error describing format: {msg}"

  -- Serializable snapshots of Lean elaboration state
  let s ← LeanSerde.LeanSnapshot.create ["Init"]

  let s' ← s.command "theorem test : 1 + 1 = 2 := by sorry"
  IO.println s!"Goals: {← s'.goals}"

  let s'' ← s'.tactic "rfl"

  let serialized: ByteArray := ← LeanSerde.serialize s''
  let s''' ← LeanSerde.deserialize serialized
  match s''' with
  | .ok (s''': LeanSerde.LeanSnapshot) =>
    IO.println s!"Complete: {s'''.complete?}"
  | .error msg => IO.println s!"Deserialization failed: {msg}"
