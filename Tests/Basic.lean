import LeanSerial


structure TestData where
  name : String
  value : Nat
  deriving Serialize, DecidableEq


def test_roundtrip {α} [Serialize α] [DecidableEq α] (testName: String) (value : α) : IO Unit := do
  let bytes ← LeanSerial.serialize value
  let deserializedValue ← LeanSerial.deserialize bytes
  if value == deserializedValue then
    IO.println s!"OK {testName}"
  else
    IO.println s!"Failed roundtrip {testName}"


def test_basic_types : IO Unit := do
  -- test_roundtrip "String 1" "hello"
  -- test_roundtrip "String 2" "Hello, Lean!"
  -- test_roundtrip "Nat" 42
  test_roundtrip "Bool" true
  -- test_roundtrip "Int" (-123)
  IO.println "Running basic type tests..."

def test_structures : IO Unit := do
  -- Custom structures
  let testData := TestData.mk "Test" 100
  test_roundtrip "Structure" testData


def main : IO Unit := do
  IO.println "Running serialization tests..."
  test_basic_types
  test_structures
  IO.println "All tests completed."
