import Tests.TestFramework
import LeanSerde.MetaTypes
import LeanSerde.SnapshotTypes

open LeanSerde TestFramework
namespace GraphTests

private structure Raw where
  value : SerialValue
private instance : Serializable Raw where
  encode value := pure value.value
  decode value := pure ⟨value⟩

private def invalidGraph (name : String) (graph : GraphData) : IO TestResult := do
  match ← (decodeGraph graph : DecodeM Nat).run with
  | .error _ => return .success name
  | .ok _ => return .failure name "Malformed graph was accepted"

private def wireFormat : IO TestResult := do
  let child := SerialValue.compound "child" #[.nat 42]
  let root := SerialValue.compound "root" #[child, child]
  let graph ← encodeGraph (Raw.mk root)
  if graph.root == .compound "root" #[.ref 0, .ref 0] && graph.objects == #[child] then
    let decoded ← (decodeGraph graph : DecodeM Raw).run
    match decoded with
    | .ok value =>
      if value.value == root then return .success "Shared graph wire format"
      else return .failure "Shared graph wire format" "Roundtrip changed the value"
    | .error e => return .failure "Shared graph wire format" e
  else return .failure "Shared graph wire format" "Reference IDs or object order changed"

private def options : IO TestResult := do
  let original := Lean.Options.empty.setBool `trace.test true |>.set `maxRecDepth (123 : Nat)
  let graph ← encodeGraph original
  match ← (decodeGraph graph : DecodeM Lean.Options).run with
  | .ok decoded =>
    if decoded == original && decoded.hasTrace then return .success "Options and trace cache"
    else return .failure "Options and trace cache" "Options changed"
  | .error e => return .failure "Options and trace cache" e

private def snapshotVersion : IO TestResult := do
  let graph : GraphData := ⟨.compound "LeanSnapshot.v1" #[.str "incompatible", .nat 0], #[]⟩
  match ← (decodeGraph graph : DecodeM LeanSnapshot).run with
  | .error e =>
    if e.startsWith "Snapshot requires Lean" then return .success "Snapshot version rejection"
    else return .failure "Snapshot version rejection" e
  | .ok _ => return .failure "Snapshot version rejection" "Wrong Lean version was accepted"

def run : IO Bool := runTests "Graph and migration regressions" [
  invalidGraph "Self-cycle" ⟨.ref 0, #[.ref 0]⟩,
  invalidGraph "Indirect cycle" ⟨.ref 0, #[.ref 1, .ref 0]⟩,
  invalidGraph "Out-of-range reference" ⟨.ref 1, #[.nat 0]⟩,
  wireFormat, options, snapshotVersion,
  test_roundtrip "Negative timestamp" (Std.Time.Timestamp.ofNanosecondsSinceUnixEpoch ⟨-1234567890⟩),
  test_roundtrip "Subsecond timestamp" (Std.Time.Timestamp.ofNanosecondsSinceUnixEpoch ⟨1234567890⟩)
]
end GraphTests
