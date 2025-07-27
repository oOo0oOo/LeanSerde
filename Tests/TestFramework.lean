import LeanSerde

open LeanSerde

namespace TestFramework

structure TestResult where
  name : String
  passed : Bool
  error : Option String
  deriving Repr

def TestResult.success (name : String) : TestResult :=
  { name := name, passed := true, error := none }

def TestResult.failure (name : String) (error : String) : TestResult :=
  { name := name, passed := false, error := some error }

def test_roundtrip {α} [Serializable α] [BEq α] (testName: String) (value : α) : IO TestResult := do
  try
    -- Test ByteArray format
    let bytes: ByteArray := serialize value
    match deserialize bytes with
    | .error e =>
      return TestResult.failure testName s!"Failed to deserialize ByteArray: {e}"
    | .ok deserializedValue =>
      if !(value == deserializedValue) then
        return TestResult.failure testName "Value mismatch after ByteArray roundtrip"

    -- Test JSON format
    let json: Lean.Json := serialize value
    match deserialize json with
    | .error e =>
      return TestResult.failure testName s!"Failed to deserialize JSON: {e}"
    | .ok deserializedValue =>
      if !(value == deserializedValue) then
        return TestResult.failure testName "Value mismatch after JSON roundtrip"

    -- Test String format
    let str: String := serialize value
    match deserialize str with
    | .error e =>
      return TestResult.failure testName s!"Failed to deserialize String: {e}"
    | .ok deserializedValue =>
      if !(value == deserializedValue) then
        return TestResult.failure testName "Value mismatch after String roundtrip"

    -- Test to/from file JSON
    let filePath := s!"{testName}.test"
    LeanSerde.serializeToJsonFile value filePath
    let fileValue : Except String α ← LeanSerde.deserializeFromJsonFile filePath
    match fileValue with
    | .error e =>
      return TestResult.failure testName s!"Failed to deserialize from JSON file: {e}"
    | .ok deserializedValue =>
      if !(value == deserializedValue) then
        return TestResult.failure testName "Value mismatch after file JSON roundtrip"

    -- Test to/from file CBOR
    LeanSerde.serializeToFile value filePath
    let fileValueCBOR : Except String α ← LeanSerde.deserializeFromFile filePath
    match fileValueCBOR with
    | .error e =>
      return TestResult.failure testName s!"Failed to deserialize from CBOR file: {e}"
    | .ok deserializedValue =>
      if !(value == deserializedValue) then
        return TestResult.failure testName "Value mismatch after CBOR file roundtrip"

    -- Clean up test file
    _ ← IO.FS.removeFile filePath

    return TestResult.success testName
  catch e =>
    return TestResult.failure testName s!"Exception: {e}"


def runTests (suiteName : String) (tests : List (IO TestResult)) : IO Bool := do
  IO.println s!"Running {suiteName}..."
  let results ← tests.mapM id

  let passed := results.filter (·.passed)
  let failed := results.filter (fun r => !r.passed)

  for result in passed do
    IO.println s!"  ✓ {result.name}"

  for result in failed do
    let errorMsg := result.error.getD "Unknown error"
    IO.println s!"  ✗ {result.name}: {errorMsg}"

  IO.println s!"  {passed.length}/{results.length} tests passed"

  return failed.isEmpty

end TestFramework
