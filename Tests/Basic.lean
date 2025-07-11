import LeanSerial

open LeanSerial


def test_roundtrip {α} [Serializable α] [DecidableEq α] (testName: String) (value : α) : IO Unit := do
  let bytes := serialize value
  match deserialize bytes with
  | .error e => IO.println s!"Failed to deserialize {testName}: {e}"
  | .ok deserializedValue =>
    if value == deserializedValue then
      IO.println s!"OK {testName}"
    else
      IO.println s!"Failed roundtrip {testName}: value mismatch"


-- Test basic types
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
  test_roundtrip "Nested Option" (some (some 42) : Option (Option Nat))


-- Test custom structures
structure TestData where
  name : String
  value : Nat
  deriving Serializable, DecidableEq

structure TestData2 where
  id : Nat
  data : TestData
  flag : Bool
  -- flag2 : Bool := false
  deriving Serializable, DecidableEq

structure TestData3 where
  items : List TestData
  items2 : Array TestData2
  -- optionalField : Option String := none
  deriving Serializable, DecidableEq

structure TestData4 where
  nested : TestData3
  flag : Bool
  deriving Serializable, DecidableEq

def test_structures : IO Unit := do
  IO.println "Running structure tests..."

  let testData := TestData.mk "Test" 100
  test_roundtrip "Structure" testData

  let testData2 := TestData2.mk 1 testData true
  test_roundtrip "Nested Structure" testData2

  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2]
  test_roundtrip "Complex Structure" testData3

  let testData4 := TestData4.mk testData3 true
  test_roundtrip "Deeply Nested Structure" testData4


-- Test inductive types
inductive TestInductive
| case1 : Nat → TestInductive
| case2 : String → TestInductive
| case3 : Bool → TestInductive
deriving Serializable, DecidableEq

inductive TestInductive2
| single : Nat → TestInductive2
| multi : String → Bool → TestInductive2
| complex : Nat → Bool → String → Bool → Bool → Bool → String → Bool → Bool → TestInductive2
deriving Serializable, DecidableEq

def test_inductive : IO Unit := do
  IO.println "Running inductive type tests..."
  let value := TestInductive.case1 42
  test_roundtrip "Inductive Type" value

  let value2 := TestInductive2.multi "test" true
  test_roundtrip "Inductive Type with Multiple Constructors" value2

  let complexValue := TestInductive2.complex 1 true "test" false true false "another" true false
  test_roundtrip "Complex Inductive Type" complexValue


def main : IO Unit := do
  IO.println "Starting LeanSerial tests..."
  test_basic_types
  test_structures
  test_inductive
  IO.println "All tests completed."
