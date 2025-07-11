import Std.Data.HashMap
import Std.Data.HashSet

import LeanSerial

open LeanSerial


-- Add BEq for various types to support equality checks in tests
instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .ok a, .ok b => a == b
    | .error e1, .error e2 => e1 == e2
    | _, _ => false


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
  IO.println ""
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

  -- String, Char, Filepath
  test_roundtrip "String" "Hello, Lean!"
  test_roundtrip "Char" 'A'
  test_roundtrip "FilePath Unix" ("/usr/bin/lean" : System.FilePath)
  test_roundtrip "FilePath Windows" ("C:\\Program Files\\Lean\\lean.exe" : System.FilePath)

  -- Boolean and Option
  test_roundtrip "Bool" true
  test_roundtrip "Bool False" false
  test_roundtrip "Option Some" (some 42)
  test_roundtrip "Option None" (none : Option Nat)
  test_roundtrip "Nested Option" (some (some 42) : Option (Option Nat))

  -- Various Functional Types
  test_roundtrip "Unit" ()
  test_roundtrip "Except" (Except.ok 42 : Except String Nat)
  test_roundtrip "Except Error" (Except.error "Error message" : Except String Nat)

def test_thunk : IO Unit := do
  let thunk : Unit → Nat := fun _ => 42
  let bytes := serialize thunk
  match (deserialize bytes : Except String (Unit → Nat)) with
  | .error e => IO.println s!"Failed to deserialize Thunk: {e}"
  | .ok deserialized =>
    if deserialized () == thunk () then
      IO.println "OK Thunk"
    else
      IO.println "Failed roundtrip Thunk: value mismatch"


def test_container_types : IO Unit := do
  IO.println ""
  IO.println "Running container type tests..."
  -- Tuples
  test_roundtrip "Prod" (42, "Test")
  test_roundtrip "Prod 2" ('B', 3.12)
  test_roundtrip "Triple" (1, "Triple", 3)

  -- Sum Types
  test_roundtrip "Sum Left" (Sum.inl 42 : Sum Nat String)
  test_roundtrip "Sum Right" (Sum.inr "Hello" : Sum Nat String)
  test_roundtrip "Sum Nested" (Sum.inl (Sum.inr "Nested") : Sum (Sum Nat String) String)

  -- Container Types
  test_roundtrip "List" ([1, 2, 3] : List Nat)
  test_roundtrip "Nested List" ([[2,3], [4,5,6],[7],[8,9,10]] : List (List Nat))
  test_roundtrip "Array" (#[4, 5, 6] : Array Nat)
  test_roundtrip "Fin 3" (Fin.ofNat (n := 3) 2)
  test_roundtrip "Fin 100" (Fin.ofNat (n := 100) 99)

def test_subarray : IO Unit := do
  let arr := #[1, 2, 3, 4, 5]
  let sub := arr.toSubarray
  let bytes := serialize sub
  match (deserialize bytes : Except String (Subarray Nat)) with
  | .error e => IO.println s!"Failed to deserialize Subarray: {e}"
  | .ok deserialized =>
    if sub.toArray == deserialized.toArray then
      IO.println "OK Subarray"
    else
      IO.println "Failed roundtrip Subarray: value mismatch"

def test_bytearray : IO Unit := do
  let ba1 := ByteArray.mk #[1, 2, 3]
  let bytes := serialize ba1
  match (deserialize bytes : Except String ByteArray) with
  | .error e => IO.println s!"Failed to deserialize ByteArray: {e}"
  | .ok ba2 =>
    if ba1.data == ba2.data then
      IO.println "OK ByteArray"
    else
      IO.println "Failed roundtrip ByteArray: value mismatch"

  let empty := ByteArray.mk #[]
  let bytes2 := serialize empty
  match (deserialize bytes2 : Except String ByteArray) with
  | .error e => IO.println s!"Failed to deserialize ByteArray Empty: {e}"
  | .ok ba3 =>
    if empty.data == ba3.data then
      IO.println "OK ByteArray Empty"
    else
      IO.println "Failed roundtrip ByteArray Empty: value mismatch"

def test_hashmap : IO Unit := do
  let hm1 := Std.HashMap.ofList [(1, "one"), (2, "two"), (3, "three")]
  let bytes := serialize hm1
  match (deserialize bytes : Except String (Std.HashMap Nat String)) with
  | .error e => IO.println s!"Failed to deserialize HashMap: {e}"
  | .ok hm2 =>
    let list1 := hm1.toList.toArray.qsort (fun a b => a.1 < b.1)
    let list2 := hm2.toList.toArray.qsort (fun a b => a.1 < b.1)
    if list1.size == list2.size &&
       (List.range list1.size).all (fun i => list1[i]! == list2[i]!) then
      IO.println "OK HashMap"
    else
      IO.println "Failed roundtrip HashMap: value mismatch"

def test_hashset : IO Unit := do
  let hs1 := Std.HashSet.ofList [1, 2, 3]
  let bytes := serialize hs1
  match (deserialize bytes : Except String (Std.HashSet Nat)) with
  | .error e => IO.println s!"Failed to deserialize HashSet: {e}"
  | .ok hs2 =>
    let list1 := hs1.toList.toArray.qsort (· < ·)
    let list2 := hs2.toList.toArray.qsort (· < ·)
    if list1.size == list2.size &&
       (List.range list1.size).all (fun i => list1[i]! == list2[i]!) then
      IO.println "OK HashSet"
    else
      IO.println "Failed roundtrip HashSet: value mismatch"

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
  metadata : Option (String × Nat) := none
  deriving Serializable, BEq

def test_structures : IO Unit := do
  IO.println ""
  IO.println "Running structure tests..."

  let testData := TestData.mk "Test" 100
  test_roundtrip "Structure" testData

  let testData2 := TestData2.mk 1 testData true false
  test_roundtrip "Nested Structure" testData2

  let testData3 := TestData3.mk [testData, TestData.mk "Another" 200] #[testData2] "Opt"
  test_roundtrip "Complex Structure" testData3

  let testData4 := TestData4.mk testData3 (none : Option (String × Nat))
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
  IO.println ""
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
  test_thunk
  test_container_types
  test_subarray
  test_bytearray
  test_hashmap
  test_hashset
  test_structures
  test_inductive
  IO.println "All tests completed."
