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
    match stringResult, natResult, boolResult with
    | .ok sJson, .ok nJson, .ok bJson =>
      if sJson == .str "<String>" && nJson == .str "<Nat>" && bJson == .str "<Bool>" then
        return TestResult.success "Primitive types"
      else
        return TestResult.failure "Primitive types" s!"Unexpected format: {sJson}, {nJson}, {bJson}"
    | .error err, _, _ => return TestResult.failure "Primitive types" s!"String error: {err}"
    | _, .error err, _ => return TestResult.failure "Primitive types" s!"Nat error: {err}"
    | _, _, .error err => return TestResult.failure "Primitive types" s!"Bool error: {err}"
  catch e =>
    return TestResult.failure "Primitive types" s!"Exception: {e}"

-- Test simple structure
def test_describe_simple_structure : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(TestData)
    match result with
    | .ok json =>
      let expected := .arr #[.str "TestData", .arr #[.str "<String>", .str "<Nat>", .str "<Bool>", .arr #[.str "List.cons", .arr #[.str "<?>"]]]]
      if json == expected then
        return TestResult.success "Simple structure"
      else
        return TestResult.failure "Simple structure" s!"Expected: {expected}, got: {json}"
    | .error err =>
      return TestResult.failure "Simple structure" s!"Error: {err}"
  catch e =>
    return TestResult.failure "Simple structure" s!"Exception: {e}"

-- Test complex structure with containers
def test_describe_complex_structure : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(ComplexData)
    match result with
    | .ok json =>
      let expected := .arr #[.str "ComplexData", .arr #[.str "<Nat>", .arr #[.str "Array", .arr #[.str "<?>"]], .arr #[.str "Option.none", .arr #[.str "<?>"]], .arr #[.str "List.cons", .arr #[.str "<?>"]]]]
      if json == expected then
        return TestResult.success "Complex structure with containers"
      else
        return TestResult.failure "Complex structure" s!"Expected: {expected}, got: {json}"
    | .error err =>
      return TestResult.failure "Complex structure" s!"Error: {err}"
  catch e =>
    return TestResult.failure "Complex structure" s!"Exception: {e}"

-- Test inductive type
def test_describe_inductive : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(Status)
    match result with
    | .ok json =>
      -- Check if it contains "pending" as a string value in the JSON
      let jsonStr := json.pretty
      if stringContains jsonStr "pending" then
        return TestResult.success "Inductive type"
      else
        return TestResult.failure "Inductive type" s!"Expected 'pending' in result: {json}"
    | .error err =>
      return TestResult.failure "Inductive type" s!"Error: {err}"
  catch e =>
    return TestResult.failure "Inductive type" s!"Exception: {e}"

-- Test container types
def test_describe_containers : IO TestResult := do
  try
    let listResult := LeanSerde.describeFormat(List String)
    let arrayResult := LeanSerde.describeFormat(Array Nat)
    let optionResult := LeanSerde.describeFormat(Option Bool)
    match listResult, arrayResult, optionResult with
    | .ok listJson, .ok arrayJson, .ok optionJson =>
      let listExpected := .arr #[.str "List.cons", .arr #[.str "<?>"]]
      let arrayExpected := .arr #[.str "Array", .arr #[.str "<?>"]]
      let optionExpected := .arr #[.str "Option.none", .arr #[.str "<?>"]]

      let listOk := listJson == listExpected
      let arrayOk := arrayJson == arrayExpected
      let optionOk := optionJson == optionExpected

      if listOk && arrayOk && optionOk then
        return TestResult.success "Container types"
      else
        return TestResult.failure "Container types" s!"List: {listJson}, Array: {arrayJson}, Option: {optionJson}"
    | .error err, _, _ => return TestResult.failure "Container types" s!"List error: {err}"
    | _, .error err, _ => return TestResult.failure "Container types" s!"Array error: {err}"
    | _, _, .error err => return TestResult.failure "Container types" s!"Option error: {err}"
  catch e =>
    return TestResult.failure "Container types" s!"Exception: {e}"

-- Test product types
def test_describe_products : IO TestResult := do
  try
    let pairResult := LeanSerde.describeFormat(String × Nat)
    match pairResult with
    | .ok json =>
      let jsonStr := json.pretty
      if stringContains jsonStr "Prod" then
        return TestResult.success "Product types"
      else
        return TestResult.failure "Product types" s!"Unexpected result: {json}"
    | .error err =>
      return TestResult.failure "Product types" s!"Error: {err}"
  catch e =>
    return TestResult.failure "Product types" s!"Exception: {e}"

-- Test non-serializable type
def test_describe_non_serializable : IO TestResult := do
  try
    let result := LeanSerde.describeFormat(Float → String)
    match result with
    | .ok json =>
      let jsonStr := json.pretty
      if stringContains jsonStr "(Not Serializable)" then
        return TestResult.success "Non-serializable type"
      else
        return TestResult.failure "Non-serializable type" s!"Expected '(Not Serializable)', got: {json}"
    | .error err =>
      if stringContains err "not Serializable" then
        return TestResult.success "Non-serializable type"
      else
        return TestResult.failure "Non-serializable type" s!"Expected serialization error, got: {err}"
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
