import Tests.TestFramework
import LeanSerial

open TestFramework

namespace StructureTests

-- Test structures
structure TestData where
  name : String
  value : Nat
  deriving LeanSerial.Serializable, BEq

structure TestData2 where
  id : Nat
  data : TestData
  flag : Bool
  flag2 : Bool := false
  deriving LeanSerial.Serializable, BEq

structure TestData3 where
  items : List TestData
  items2 : Array TestData2
  optionalField : Option String := none
  deriving LeanSerial.Serializable, BEq

structure TestData4 where
  nested : TestData3
  metadata : Option (String × Nat) := none
  deriving LeanSerial.Serializable, BEq

def test_simple_structure : IO TestResult := do
  let testData := TestData.mk "Test" 100
  test_roundtrip "Simple Structure" testData

def test_nested_structure : IO TestResult := do
  let testData := TestData.mk "Test" 100
  let testData2 := TestData2.mk 1 testData true false
  test_roundtrip "Nested Structure" testData2

def test_complex_structure : IO TestResult := do
  let testData := TestData.mk "Test" 100
  let testData2 := TestData2.mk 1 testData true false
  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2] "Opt"
  test_roundtrip "Complex Structure" testData3

def test_deeply_nested_structure : IO TestResult := do
  let testData := TestData.mk "Test" 100
  let testData2 := TestData2.mk 1 testData true false
  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2] "Opt"
  let testData4 := TestData4.mk testData3 (none : Option (String × Nat))
  test_roundtrip "Deeply Nested Structure" testData4

def run: IO Bool := do
  runTests "Structure Serialization" [
    test_simple_structure,
    test_nested_structure,
    test_complex_structure,
    test_deeply_nested_structure
  ]

end StructureTests

namespace InductiveTests

-- Test inductive types
inductive TestInductive
| true
| false
| none
| runtime
| comptime
| all
deriving LeanSerial.Serializable, BEq

inductive TestInductive2
| single : Nat → TestInductive2
| multi : String → Bool → TestInductive2
| complex : Nat → Bool → String → Bool → Bool → Bool → String → Bool → Bool → TestInductive2
deriving LeanSerial.Serializable, BEq

inductive TestInductive3
| empty : TestInductive3
| case1 : Nat → TestInductive3
| case2 : String → TestInductive3
| case3 : Bool → TestInductive3
| case4 : Nat → String → TestInductive3
deriving LeanSerial.Serializable, BEq

-- Mutually recursive inductive
inductive TestInductive5
| leaf : TestInductive5
| zero : List TestInductive5 → TestInductive5
deriving LeanSerial.Serializable, BEq

inductive TestInductive6
| case1 : TestInductive5 → TestInductive6
| case2 : Array TestInductive6 → TestInductive5 → TestInductive6
deriving LeanSerial.Serializable, BEq

inductive TestInductive7
| case1 : Nat → TestInductive7
| case2 : TestInductive7 → TestInductive7 → TestInductive7
| case3 : TestInductive6 → TestInductive7
| case4 : TestInductive7 → List TestInductive6 → TestInductive5 → TestInductive7
deriving LeanSerial.Serializable, BEq

def test_simple_inductive : IO TestResult := do
  let value := TestInductive.none
  test_roundtrip "Simple Inductive" value

def test_multi_constructor_inductive : IO TestResult := do
  let value2 := TestInductive2.multi "test" true
  test_roundtrip "Multi Constructor Inductive" value2

def test_complex_inductive : IO TestResult := do
  let complexValue := TestInductive2.complex 1 true "test" false true false "another" true false
  test_roundtrip "Complex Inductive" complexValue

def test_multiple_cases_inductive : IO TestResult := do
  let value3 := TestInductive3.case4 100 "example"
  test_roundtrip "Multiple Cases Inductive" value3

def test_recursive_inductive : IO TestResult := do
  let value5 := TestInductive5.zero ([TestInductive5.leaf])
  test_roundtrip "Recursive Inductive" value5

def test_mutually_recursive_inductive : IO TestResult := do
  let value6 := TestInductive6.case1 (TestInductive5.zero ([TestInductive5.leaf]))
  test_roundtrip "Mutually Recursive Inductive" value6

def test_recursive_inductive2 : IO TestResult := do
  let value5 := TestInductive5.zero ([TestInductive5.leaf])
  let value6 := TestInductive6.case1 value5
  let value7 := TestInductive7.case3 value6
  let value8 := TestInductive7.case4 value7 [value6] value5
  test_roundtrip "Recursive Inductive 2" value8

def run: IO Bool := do
  runTests "Inductive Type Serialization" [
    test_simple_inductive,
    test_multi_constructor_inductive,
    test_complex_inductive,
    test_multiple_cases_inductive,
    test_recursive_inductive,
    test_mutually_recursive_inductive,
    test_recursive_inductive2
  ]

end InductiveTests
