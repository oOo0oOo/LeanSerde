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

structure TestContainers where
  optionalValue : Option String
  pair : Nat × String
  choice : String ⊕ Nat
  listOfOptions : List (Option Nat)
  arrayOfPairs : Array (Nat × Bool)
  optionalPair : Option (String × Nat)
  deriving LeanSerial.Serializable, BEq

-- Cyclic
inductive Tree where
  | leaf : Nat → Tree
  | node : Tree → Tree → Tree
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

def test_container_types : IO TestResult := do
  let testData := TestContainers.mk
    (some "hello")
    (42, "world")
    (Sum.inl "left")
    [some 1, none, some 3]
    #[(1, true), (2, false)]
    (some ("test", 100))
  test_roundtrip "Container Types" testData

def test_container_types_with_nones : IO TestResult := do
  let testData := TestContainers.mk
    none
    (0, "empty")
    (Sum.inr 42)
    [none, none, some 5]
    #[]
    none
  test_roundtrip "Container Types with Nones" testData

def test_cyclic_tree : IO TestResult := do
  -- Create a mutable reference that can form a cycle
  let leafRef ← IO.mkRef (Tree.leaf 1)
  let nodeRef ← IO.mkRef (Tree.leaf 2) -- temporary value

  let cycleNode := Tree.node (Tree.leaf 1) (← nodeRef.get)
  nodeRef.set cycleNode

  let cyclicTree := Tree.node (← leafRef.get) (← nodeRef.get)
  test_roundtrip "Cyclic Tree" cyclicTree

def test_shared_references : IO TestResult := do
  -- Create a shared leaf that appears in multiple places
  let sharedLeaf := Tree.leaf 42

  -- Create a tree where the same leaf appears multiple times
  let leftSubtree := Tree.node sharedLeaf (Tree.leaf 1)
  let rightSubtree := Tree.node sharedLeaf (Tree.leaf 2)
  let complexTree := Tree.node leftSubtree rightSubtree

  test_roundtrip "Shared References Tree" complexTree

def run: IO Bool := do
  runTests "Inductive Type Serialization" [
    test_simple_inductive,
    test_multi_constructor_inductive,
    test_complex_inductive,
    test_multiple_cases_inductive,
    test_recursive_inductive,
    test_mutually_recursive_inductive,
    test_recursive_inductive2,
    test_container_types,
    test_container_types_with_nones,
    test_cyclic_tree,
    test_shared_references
  ]

end InductiveTests

namespace PolymorphicTests

-- Test polymorphic types
inductive MyPair (α β : Type) where
  | mk (a : α) (b : β)
  deriving LeanSerial.Serializable, BEq

inductive MyOption (α : Type) where
  | none
  | some (value : α)
  deriving LeanSerial.Serializable, BEq

inductive MyList (α : Type) where
  | nil
  | cons (head : α) (tail : MyList α)
  deriving LeanSerial.Serializable, BEq

structure Container (α β : Type) where
  first : α
  second : β
  items : List α
  deriving LeanSerial.Serializable, BEq

-- Test instances
def test_polymorphic_pair : IO TestResult := do
  let testPair : MyPair String Nat := MyPair.mk "hello" 42
  test_roundtrip "Polymorphic Pair" testPair

def test_polymorphic_option_some : IO TestResult := do
  let testOption : MyOption String := MyOption.some "test"
  test_roundtrip "Polymorphic Option Some" testOption

def test_polymorphic_option_none : IO TestResult := do
  let testOption : MyOption String := MyOption.none
  test_roundtrip "Polymorphic Option None" testOption

def test_polymorphic_list : IO TestResult := do
  let testList : MyList Nat :=
    MyList.cons 1 (MyList.cons 2 (MyList.cons 3 MyList.nil))
  test_roundtrip "Polymorphic List" testList

def test_polymorphic_container : IO TestResult := do
  let container : Container String Nat := {
    first := "hello",
    second := 42,
    items := ["a", "b", "c"]
  }
  test_roundtrip "Polymorphic Container" container

def test_nested_polymorphic : IO TestResult := do
  let nested : MyPair (MyOption String) (MyList Nat) :=
    MyPair.mk (MyOption.some "test") (MyList.cons 1 (MyList.cons 2 MyList.nil))
  test_roundtrip "Nested Polymorphic" nested

-- Test with multiple type parameters
inductive Triple (α β γ : Type) where
  | mk (a : α) (b : β) (c : γ)
  deriving LeanSerial.Serializable, BEq

def test_triple : IO TestResult := do
  let triple : Triple String Nat Bool := Triple.mk "hello" 42 true
  test_roundtrip "Triple Type Parameters" triple

def run : IO Bool := do
  runTests "Polymorphic Type Serialization" [
    test_polymorphic_pair,
    test_polymorphic_option_some,
    test_polymorphic_option_none,
    test_polymorphic_list,
    test_polymorphic_container,
    test_nested_polymorphic,
    test_triple
  ]

end PolymorphicTests
