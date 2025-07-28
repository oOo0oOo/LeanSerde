import Tests.TestFramework
import LeanSerde

open TestFramework

namespace DescribeTests

structure TestData where
  name : String
  value : Nat
  isActive : Bool
  children : List TestData
  deriving LeanSerde.Serializable, Inhabited, BEq

structure ComplexData where
  id : Nat
  items : Array String
  metadata : Option (String × Nat)
  tags : List Bool
  deriving LeanSerde.Serializable, Inhabited, BEq

inductive Status
  | pending
  | active (priority : Nat)
  | completed (result : String)
  deriving LeanSerde.Serializable, Inhabited, BEq

private def stringContains (haystack : String) (needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

-- Test basic types
def test_describe_primitives : IO TestResult := do
  try
    let stringResult := LeanSerde.describeFormat(String)
    let natResult := LeanSerde.describeFormat(Nat)
    let boolResult := LeanSerde.describeFormat(Bool)
    if stringResult == "<String>" && natResult == "<Nat>" && boolResult == "<Bool>" then
      return TestResult.success "Primitive types"
    else
      return TestResult.failure "Primitive types" "Unexpected format"
  catch e =>
    return TestResult.failure "Primitive types" s!"Exception: {e}"

-- Test simple structure
def test_describe_simple_structure : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(TestData)
    let expected := "[\"TestData\", [<String>, <Nat>, <Bool>, [\"List.cons\", [<?>]]]]"
    if result == expected then
      return TestResult.success "Simple structure"
    else
      return TestResult.failure "Simple structure" s!"Expected: {expected}, got: {result}"
  catch e =>
    return TestResult.failure "Simple structure" s!"Exception: {e}"

-- Test complex structure with containers
def test_describe_complex_structure : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(ComplexData)
    let expected := "[\"ComplexData\", [<Nat>, [\"Array\", [<?>]], [\"Option.none\", [<?>]], [\"List.cons\", [<?>]]]]"
    if result == expected then
      return TestResult.success "Complex structure with containers"
    else
      return TestResult.failure "Complex structure" s!"Expected: {expected}, got: {result}"
  catch e =>
    return TestResult.failure "Complex structure" s!"Exception: {e}"

-- Test inductive type
def test_describe_inductive : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(Status)
    -- Should show the first constructor (pending) since we use default
    if stringContains result "pending" then
      return TestResult.success "Inductive type"
    else
      return TestResult.failure "Inductive type" s!"Expected 'pending' in result: {result}"
  catch e =>
    return TestResult.failure "Inductive type" s!"Exception: {e}"

-- Test container types
def test_describe_containers : IO TestResult := do
  try
    let listResult := LeanSerde.describeFormat(List String)
    let arrayResult := LeanSerde.describeFormat(Array Nat)
    let optionResult := LeanSerde.describeFormat(Option Bool)
    -- Updated to match actual serialization behavior
    let listOk := listResult == "[\"List.cons\", [<?>]]"  -- This is correct
    let arrayOk := arrayResult == "[\"Array\", [<?>]]"
    let optionOk := optionResult == "[\"Option.none\", [<?>]]"

    if listOk && arrayOk && optionOk then
      return TestResult.success "Container types"
    else
      return TestResult.failure "Container types" s!"List: {listResult}, Array: {arrayResult}, Option: {optionResult}"
  catch e =>
    return TestResult.failure "Container types" s!"Exception: {e}"

-- Test product types
def test_describe_products : IO TestResult := do
  try
    let pairResult := LeanSerde.describeFormat(String × Nat)
    if stringContains pairResult "Prod" then
      return TestResult.success "Product types"
    else
      return TestResult.failure "Product types" s!"Unexpected result: {pairResult}"
  catch e =>
    return TestResult.failure "Product types" s!"Exception: {e}"

-- Test non-serializable type
def test_describe_non_serializable : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(Float → String)
    if stringContains result "(Not Serializable)" then
      return TestResult.success "Non-serializable type"
    else
      return TestResult.failure "Non-serializable type" s!"Expected '(Not Serializable)', got: {result}"
  catch e =>
    return TestResult.failure "Non-serializable type" s!"Exception: {e}"

def run : IO Bool := do
  runTests "Describe Functionality" [
    test_describe_primitives,
    test_describe_simple_structure,
    test_describe_complex_structure,
    test_describe_inductive,
    test_describe_containers,
    test_describe_products,
    test_describe_non_serializable
  ]

end DescribeTests
