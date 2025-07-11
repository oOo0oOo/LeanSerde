import LeanSerial

open LeanSerial


structure TestData where
  name : String
  value : Nat
  deriving Serializable, DecidableEq

structure TestData2 where
  id : Nat
  data : TestData
  deriving Serializable, DecidableEq

structure TestData3 where
  items : List TestData
  items2 : Array TestData2
  deriving Serializable, DecidableEq

def test_roundtrip {α} [Serializable α] [DecidableEq α] (testName: String) (value : α) : IO Unit := do
  let bytes := serialize value
  match deserialize bytes with
  | .error e => IO.println s!"Failed to deserialize {testName}: {e}"
  | .ok deserializedValue =>
    if value == deserializedValue then
      IO.println s!"OK {testName}"
    else
      IO.println s!"Failed roundtrip {testName}: value mismatch"


def test_basic_types : IO Unit := do
  IO.println "Running basic type tests..."
  test_roundtrip "String 1" "hello"
  test_roundtrip "String 2" "Hello, Lean!"
  test_roundtrip "Nat" 42
  test_roundtrip "Bool" true
  test_roundtrip "Int" (-123)
  test_roundtrip "Option Some" (some 42)
  test_roundtrip "Option None" (none : Option Nat)
  test_roundtrip "List" ([1, 2, 3] : List Nat)
  test_roundtrip "Array" (#[4, 5, 6] : Array Nat)

def test_structures : IO Unit := do
  IO.println "Running structure tests..."

  let testData := TestData.mk "Test" 100
  test_roundtrip "Structure" testData

  let testData2 := TestData2.mk 1 testData
  test_roundtrip "Nested Structure" testData2

  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2]
  test_roundtrip "Complex Structure" testData3


def main : IO Unit := do
  IO.println "Starting LeanSerial tests..."
  test_basic_types
  test_structures
  IO.println "All tests completed."
