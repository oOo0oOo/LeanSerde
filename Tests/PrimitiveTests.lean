import Tests.TestFramework
import LeanSerde

open TestFramework

namespace PrimitiveTests

-- More complex tests
def test_thunk : IO TestResult := do
  let thunk : Unit → Nat := fun _ => 42
  let bytes: ByteArray := LeanSerde.serialize thunk
  match (LeanSerde.deserialize bytes : Except String (Unit → Nat)) with
  | .error e => return TestResult.failure "Thunk" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    if deserialized () == thunk () then
      return TestResult.success "Thunk"
    else
      return TestResult.failure "Thunk" "Value mismatch"

def run: IO Bool := runTests "Primitive Types" [
  -- Numeric Types
  test_roundtrip "Nat" 42,
  test_roundtrip "Float" (3.141592653 : Float),

  test_roundtrip "UInt8" (255 : UInt8),
  test_roundtrip "UInt16" (65535 : UInt16),
  test_roundtrip "UInt32" (4294967295 : UInt32),
  test_roundtrip "UInt64" (18446744073709551615 : UInt64),

  test_roundtrip "Int negative" (-123),
  test_roundtrip "Int positive" (123),
  test_roundtrip "Int8 negative" (-128 : Int8),
  test_roundtrip "Int8 positive" (127 : Int8),
  test_roundtrip "Int16 negative" (-32768 : Int16),
  test_roundtrip "Int16 positive" (32767 : Int16),
  test_roundtrip "Int32 negative" (-2147483648 : Int32),
  test_roundtrip "Int32 positive" (2147483647 : Int32),
  test_roundtrip "Int64 negative" (-9223372036854775808 : Int64),
  test_roundtrip "Int64 positive" (9223372036854775807 : Int64),

  -- String, Char, Filepath, Bool
  test_roundtrip "String" "Hello, Lean!",
  test_roundtrip "Char" 'A',
  test_roundtrip "FilePath Unix" ("/usr/bin/lean" : System.FilePath),
  test_roundtrip "FilePath Windows" ("C:\\Program Files\\Lean\\lean.exe" : System.FilePath),
  test_roundtrip "Bool true" true,
  test_roundtrip "Bool false" false,
  test_roundtrip "Unit" (),

  -- Thunk
  test_thunk,
]

end PrimitiveTests
