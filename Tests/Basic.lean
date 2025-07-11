import LeanSerial

open LeanSerial


def test_roundtrip {α} [Serializable α] [BEq α] (testName: String) (value : α) : IO Unit := do
  let bytes := serialize value
  match deserialize bytes with
  | .error e => IO.println s!"Failed to deserialize {testName}: {e}"
  | .ok deserializedValue =>
    if value == deserializedValue then
      IO.println s!"OK {testName}"
    else
      IO.println s!"Failed roundtrip {testName}: value mismatch"


-- Test primitive types
def test_primitive_types : IO Unit := do
  IO.println "Running primitive tests..."

  -- Numeric Types
  test_roundtrip "Nat" 42
  test_roundtrip "Float" (3.141592653 : Float)

  test_roundtrip "UInt8" (255 : UInt8)
  test_roundtrip "UInt16" (65535 : UInt16)
  test_roundtrip "UInt32" (4294967295 : UInt32)
  test_roundtrip "UInt64" (18446744073709551615 : UInt64)

  test_roundtrip "Int -" (-123)
  test_roundtrip "Int" (123)
  test_roundtrip "Int8 -" (-128 : Int8)
  test_roundtrip "Int8" (127 : Int8)
  test_roundtrip "Int16 -" (-32768 : Int16)
  test_roundtrip "Int16" (32767 : Int16)
  test_roundtrip "Int32 -" (-2147483648 : Int32)
  test_roundtrip "Int32" (2147483647 : Int32)
  test_roundtrip "Int64 -" (-9223372036854775808 : Int64)
  test_roundtrip "Int64" (9223372036854775807 : Int64)

  -- String, Char and Tuples
  test_roundtrip "String" "Hello, Lean!"
  test_roundtrip "Char" 'A'
  test_roundtrip "Prod" (42, "Test")
  test_roundtrip "Prod 2" ('B', 3.12)
  test_roundtrip "Triple" (1, "Triple", 3)

  -- Boolean and Option
  test_roundtrip "Bool" true
  test_roundtrip "Bool False" false
  test_roundtrip "Option Some" (some 42)
  test_roundtrip "Option None" (none : Option Nat)

  -- Container Types
  test_roundtrip "List" ([1, 2, 3] : List Nat)
  test_roundtrip "Array" (#[4, 5, 6] : Array Nat)
  test_roundtrip "Nested Option" (some (some 42) : Option (Option Nat))


-- Test structures
structure TestData where
  name : String
  value : Nat
  deriving Serializable, BEq

structure TestData2 where
  id : Nat
  data : TestData
  flag : Bool
  flag2 : Bool := false
  deriving Serializable, BEq

structure TestData3 where
  items : List TestData
  items2 : Array TestData2
  optionalField : Option String := none
  deriving Serializable, BEq

structure TestData4 where
  nested : TestData3
  flag : Bool
  deriving Serializable, BEq

def test_structures : IO Unit := do
  IO.println "Running structure tests..."

  let testData := TestData.mk "Test" 100
  test_roundtrip "Structure" testData

  let testData2 := TestData2.mk 1 testData true false
  test_roundtrip "Nested Structure" testData2

  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2] "Opt"
  test_roundtrip "Complex Structure" testData3

  let testData4 := TestData4.mk testData3 true
  test_roundtrip "Deeply Nested Structure" testData4


-- Test inductive types
inductive TestInductive
| true
| false
| none
deriving Serializable, BEq

inductive TestInductive2
| single : Nat → TestInductive2
| multi : String → Bool → TestInductive2
| complex : Nat → Bool → String → Bool → Bool → Bool → String → Bool → Bool → TestInductive2
deriving Serializable, BEq

inductive TestInductive3
| empty : TestInductive3
| case1 : Nat → TestInductive3
| case2 : String → TestInductive3
| case3 : Bool → TestInductive3
| case4 : Nat → String → TestInductive3
deriving Serializable, BEq

def test_inductive : IO Unit := do
  IO.println "Running inductive type tests..."
  let value := TestInductive.none
  test_roundtrip "Inductive Type" value

  let value2 := TestInductive2.multi "test" true
  test_roundtrip "Inductive Type with Multiple Constructors" value2

  let complexValue := TestInductive2.complex 1 true "test" false true false "another" true false
  test_roundtrip "Complex Inductive Type" complexValue

  let value3 := TestInductive3.case4 100 "example"
  test_roundtrip "Inductive Type with Multiple Cases" value3


def main : IO Unit := do
  test_primitive_types
  test_structures
  test_inductive
  IO.println "All tests completed."
