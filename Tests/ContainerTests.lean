import Tests.TestFramework
import LeanSerde

open TestFramework

namespace ContainerTests

-- Add BEq to support equality checks in tests
instance [BEq ε] [BEq α] : BEq (Except ε α) where
  beq
    | .ok a, .ok b => a == b
    | .error e1, .error e2 => e1 == e2
    | _, _ => false

def test_bytearray_impl : IO TestResult := do
  let ba1 := ByteArray.mk #[1, 2, 3]
  let bytes: ByteArray ← LeanSerde.serialize ba1
  match (← LeanSerde.deserialize bytes : Except String ByteArray) with
  | .error e => return TestResult.failure "ByteArray" s!"Failed to deserialize: {e}"
  | .ok ba2 =>
    if ba1.data == ba2.data then
      return TestResult.success "ByteArray"
    else
      return TestResult.failure "ByteArray" "Value mismatch"

def test_bytearray_empty_impl : IO TestResult := do
  let empty := ByteArray.mk #[]
  let bytes: ByteArray ← LeanSerde.serialize empty
  match (← LeanSerde.deserialize bytes : Except String ByteArray) with
  | .error e => return TestResult.failure "ByteArray Empty" s!"Failed to deserialize: {e}"
  | .ok ba =>
    if empty.data == ba.data then
      return TestResult.success "ByteArray Empty"
    else
      return TestResult.failure "ByteArray Empty" "Value mismatch"

def test_subarray_impl : IO TestResult := do
  let arr := #[1, 2, 3, 4, 5]
  let sub := arr.toSubarray
  let bytes: ByteArray ← LeanSerde.serialize sub
  match (← LeanSerde.deserialize bytes : Except String (Subarray Nat)) with
  | .error e => return TestResult.failure "Subarray" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    if sub.toArray == deserialized.toArray then
      return TestResult.success "Subarray"
    else
      return TestResult.failure "Subarray" "Value mismatch"

def test_subarray : IO Unit := do
  let result ← test_subarray_impl
  if result.passed then
    IO.println "  ✓ Subarray"
  else
    IO.println s!"  ✗ Subarray: {result.error.getD "Unknown error"}"

def test_bytearray : IO Unit := do
  let result1 ← test_bytearray_impl
  let result2 ← test_bytearray_empty_impl
  if result1.passed then
    IO.println "  ✓ ByteArray"
  else
    IO.println s!"  ✗ ByteArray: {result1.error.getD "Unknown error"}"
  if result2.passed then
    IO.println "  ✓ ByteArray Empty"
  else
    IO.println s!"  ✗ ByteArray Empty: {result2.error.getD "Unknown error"}"

def run: IO Bool := do
  let results ← [
    runTests "Option Types" [
      test_roundtrip "Option Some" (some 42),
      test_roundtrip "Option None" (none : Option Nat),
      test_roundtrip "Nested Option" (some (some 42) : Option (Option Nat))
    ],

    runTests "Result Types" [
      test_roundtrip "Except Ok" (Except.ok 42 : Except String Nat),
      test_roundtrip "Except Error" (Except.error "Error message" : Except String Nat)
    ],

    runTests "Product Types" [
      test_roundtrip "Pair" (42, "Test"),
      test_roundtrip "Char Float Pair" ('B', 3.12),
      test_roundtrip "Triple" (1, "Triple", 3)
    ],

    runTests "Sum Types" [
      test_roundtrip "Sum Left" (Sum.inl 42 : Sum Nat String),
      test_roundtrip "Sum Right" (Sum.inr "Hello" : Sum Nat String),
      test_roundtrip "Sum Nested" (Sum.inl (Sum.inr "Nested") : Sum (Sum Nat String) String)
    ],

    runTests "Collections" [
      test_roundtrip "List" ([1, 2, 3] : List Nat),
      test_roundtrip "Nested List" ([[2,3], [4,5,6],[7],[8,9,10]] : List (List Nat)),
      test_roundtrip "Array" (#[4, 5, 6] : Array Nat),
      test_roundtrip "Fin 3" (Fin.ofNat (n := 3) 2),
      test_roundtrip "Fin 100" (Fin.ofNat (n := 100) 99)
    ],

    runTests "Special Containers" [
      test_subarray_impl,
      test_bytearray_impl,
      test_bytearray_empty_impl
    ]
  ].mapM id

  return results.all (· == true)

end ContainerTests
