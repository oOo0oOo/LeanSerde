import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Json
import Lean.Data.Position
import Lean.Data.RBMap
import Std.Time
import Std.Tactic

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
  IO.println "Running primitive type tests..."

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
  test_roundtrip "Bool" true
  test_roundtrip "Bool False" false
  test_roundtrip "Unit" ()

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


def test_time_types : IO Unit := do
  IO.println ""
  IO.println "Running time type tests..."

  -- Units: Era, Year, Month, Week, Weekday, Day, Hour, Minute, Second, Millisecond, Nanosecond
  test_roundtrip "Year" (Std.Time.Year.Offset.ofInt 2023)
  test_roundtrip "Month" (Std.Time.Month.Ordinal.ofNat 5)
  test_roundtrip "Week" (Std.Time.Week.Ordinal.ofNat 2)
  test_roundtrip "Weekday" (Std.Time.Weekday.ofNat 3)
  test_roundtrip "Day" (Std.Time.Day.Ordinal.ofNat 15)
  test_roundtrip "Hour" (Std.Time.Hour.Ordinal.ofNat 10 (by decide))
  test_roundtrip "Minute" (Std.Time.Minute.Ordinal.ofNat 30 (by decide))
  test_roundtrip "Second" (Std.Time.Second.Ordinal.ofNat (leap := false) 45 (by decide))
  test_roundtrip "Second Leap" (Std.Time.Second.Ordinal.ofNat (leap := true) 60 (by decide))
  test_roundtrip "Millisecond" (Std.Time.Millisecond.Ordinal.ofNat 500 (by decide))
  test_roundtrip "Nanosecond" (Std.Time.Nanosecond.Ordinal.ofNat 999_999_999 (by decide))

  test_roundtrip "Timestamp" (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨2000000000⟩)

  -- PlainDate, PlainDateTime, PlainTime
  test_roundtrip "PlainDate" (Std.Time.PlainDate.ofYearMonthDay?  2023 5 15)
  test_roundtrip "PlainTime" (Std.Time.PlainTime.ofHourMinuteSeconds 10 30 45)
  let dateTime := Std.Time.PlainDate.ofYearMonthDay? 2023 5 15
  match dateTime with
  | none => IO.println "Failed to create PlainDate"
  | some date =>
    test_roundtrip "PlainDateTime" (Std.Time.PlainDateTime.mk date (Std.Time.PlainTime.ofHourMinuteSeconds 10 30 45))

  -- TimeZone etc
  test_roundtrip "TimeZone" (Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false)

def test_zone_rules : IO Unit := do
  -- Test UTC timezone rules
  let utcRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false)
  let bytes := serialize utcRules
  match (deserialize bytes : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => IO.println s!"Failed to deserialize ZoneRules UTC: {e}"
  | .ok deserialized =>
    -- Test multiple properties for comprehensive validation
    let originalLtt := utcRules.initialLocalTimeType
    let deserializedLtt := deserialized.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       utcRules.transitions.size == deserialized.transitions.size then
      IO.println "OK ZoneRules UTC"
    else
      IO.println "Failed roundtrip ZoneRules UTC: value mismatch"

  -- Test timezone with positive offset (EST)
  let estRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨-18000⟩ "EST" "EST" false)
  let bytesEst := serialize estRules
  match (deserialize bytesEst : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => IO.println s!"Failed to deserialize ZoneRules EST: {e}"
  | .ok deserializedEst =>
    let originalLtt := estRules.initialLocalTimeType
    let deserializedLtt := deserializedEst.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       estRules.transitions.size == deserializedEst.transitions.size then
      IO.println "OK ZoneRules EST"
    else
      IO.println "Failed roundtrip ZoneRules EST: value mismatch"

  -- Test timezone with DST
  let dstRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨3600⟩ "CET" "CEST" true)
  let bytesDst := serialize dstRules
  match (deserialize bytesDst : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => IO.println s!"Failed to deserialize ZoneRules DST: {e}"
  | .ok deserializedDst =>
    let originalLtt := dstRules.initialLocalTimeType
    let deserializedLtt := deserializedDst.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       dstRules.transitions.size == deserializedDst.transitions.size then
      IO.println "OK ZoneRules DST"
    else
      IO.println "Failed roundtrip ZoneRules DST: value mismatch"

def test_zoned_datetime : IO Unit := do
  let utcZone := Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false
  let zonedDateTime := Std.Time.ZonedDateTime.ofTimestampWithZone
    (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨2000000000⟩) utcZone
  let bytes := serialize zonedDateTime
  match (deserialize bytes : Except String Std.Time.ZonedDateTime) with
  | .error e => IO.println s!"Failed to deserialize ZonedDateTime: {e}"
  | .ok deserialized =>
    if deserialized.timestamp == zonedDateTime.timestamp &&
       deserialized.timezone == zonedDateTime.timezone then
      IO.println "OK ZonedDateTime"
    else
      IO.println "Failed roundtrip ZonedDateTime: value mismatch"

def test_container_types : IO Unit := do
  IO.println ""
  IO.println "Running container type tests..."

  test_roundtrip "Option Some" (some 42)
  test_roundtrip "Option None" (none : Option Nat)
  test_roundtrip "Nested Option" (some (some 42) : Option (Option Nat))
  test_roundtrip "Except" (Except.ok 42 : Except String Nat)
  test_roundtrip "Except Error" (Except.error "Error message" : Except String Nat)

  -- Tuples
  test_roundtrip "Prod" (42, "Test")
  test_roundtrip "Prod 2" ('B', 3.12)
  test_roundtrip "Triple" (1, "Triple", 3)

  -- Sum Types
  test_roundtrip "Sum Left" (Sum.inl 42 : Sum Nat String)
  test_roundtrip "Sum Right" (Sum.inr "Hello" : Sum Nat String)
  test_roundtrip "Sum Nested" (Sum.inl (Sum.inr "Nested") : Sum (Sum Nat String) String)

  -- Lists, Arrays, Fin
  test_roundtrip "List" ([1, 2, 3] : List Nat)
  test_roundtrip "Nested List" ([[2,3], [4,5,6],[7],[8,9,10]] : List (List Nat))
  test_roundtrip "Array" (#[4, 5, 6] : Array Nat)
  test_roundtrip "Fin 3" (Fin.ofNat (n := 3) 2)
  test_roundtrip "Fin 100" (Fin.ofNat (n := 100) 99)

  -- Various
  test_roundtrip "JSON" (Lean.Json.mkObj [("key", Lean.Json.str "value")])
  test_roundtrip "JSON Complex" (Lean.Json.arr #[Lean.Json.str "value1", Lean.Json.str "value2", Lean.Json.num 42, Lean.Json.bool true, Lean.Json.null])
  test_roundtrip "Position" (Lean.Position.mk 1 2)

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

def test_rbmap : IO Unit := do
  let rb1 : Lean.RBMap Nat String compare :=
    Lean.RBMap.empty.insert 1 "one" |>.insert 2 "two" |>.insert 3 "three"
  let bytes := serialize rb1
  match (deserialize bytes : Except String (Lean.RBMap Nat String compare)) with
  | .error e => IO.println s!"Failed to deserialize RBMap: {e}"
  | .ok rb2 =>
    let list1 := rb1.toList.toArray.qsort (fun a b => a.1 < b.1)
    let list2 := rb2.toList.toArray.qsort (fun a b => a.1 < b.1)
    if list1.size == list2.size &&
       (List.range list1.size).all (fun i => list1[i]! == list2[i]!) then
      IO.println "OK RBMap"
    else
      IO.println "Failed roundtrip RBMap: value mismatch"

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
  test_time_types
  test_zone_rules
  test_zoned_datetime
  test_container_types
  test_subarray
  test_bytearray
  test_hashmap
  test_hashset
  test_rbmap
  test_structures
  test_inductive
  IO.println "All tests completed."
