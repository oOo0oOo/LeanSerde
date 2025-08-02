import Lean
import Tests.TestFramework
import LeanSerde
import LeanSerde.SnapshotTypes

open TestFramework Lean Elab Meta Term Command Tactic LeanSerde

namespace SnapshotTests

def test_empty_snapshot : IO TestResult := do
  let snapshot ← LeanSnapshot.empty
  let bytes: ByteArray ← LeanSerde.serialize snapshot
  match (← LeanSerde.deserialize bytes : Except String LeanSnapshot) with
  | .error e => return TestResult.failure "Empty LeanSnapshot" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    if snapshot.env.mainModule == decoded.env.mainModule &&
       snapshot.rootGoals.length == decoded.rootGoals.length then
      return TestResult.success "Empty LeanSnapshot"
    else
      return TestResult.failure "Empty LeanSnapshot" "Content mismatch"

def test_env_snapshot : IO TestResult := do
  let env ← LeanSerde.EnvironmentBuilder.minimal
  let snapshot := LeanSnapshot.fromEnv env
  let bytes: ByteArray ← LeanSerde.serialize snapshot
  match (← LeanSerde.deserialize bytes : Except String LeanSnapshot) with
  | .error e => return TestResult.failure "Environment LeanSnapshot" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    if snapshot.env.mainModule == decoded.env.mainModule &&
       snapshot.rootGoals.length == decoded.rootGoals.length then
      return TestResult.success "Environment LeanSnapshot"
    else
      return TestResult.failure "Environment LeanSnapshot" "Content mismatch"

def test_lean_context : IO TestResult := do
  let ctx : LeanContext := {}
  let bytes: ByteArray ← LeanSerde.serialize ctx
  match (← LeanSerde.deserialize bytes : Except String LeanContext) with
  | .error e => return TestResult.failure "LeanContext" s!"Failed to deserialize: {e}"
  | .ok _decoded =>
    return TestResult.success "LeanContext"

def test_command_with_sorry : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test : 1 + 1 = 2 := by sorry"
  if !snapshot2.complete? then
    return TestResult.success "Command with sorry"
  else
    return TestResult.failure "Command with sorry" "Expected goals from sorry"

def test_tactic_application : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test : True := by sorry"
  let snapshot3 ← snapshot2.tactic "trivial"
  if snapshot3.complete? then
    return TestResult.success "Tactic application"
  else
    return TestResult.failure "Tactic application" "Expected theorem to be complete after trivial"

def test_goals_formatting : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test (n : Nat) : n + 0 = n := by sorry"
  let goalStrings ← snapshot2.goals
  if goalStrings.length == 1 && (goalStrings.head!.splitOn "n + 0 = n").length > 1 then
    return TestResult.success "Goals formatting"
  else
    return TestResult.failure "Goals formatting" s!"Expected goal with 'n + 0 = n', got: {goalStrings}"

def test_multiple_goals : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test : True ∧ True := ⟨by sorry, by sorry⟩"
  let goalCount := snapshot2.rootGoals.length
  if goalCount == 2 then
    return TestResult.success "Multiple goals"
  else
    return TestResult.failure "Multiple goals" s!"Expected 2 goals, got {goalCount}"

def test_complex_theorem : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test_and (p q : Prop) (hp : p) (hq : q) : p ∧ q := by sorry"

  if snapshot2.complete? then
    return TestResult.failure "Complex theorem" "Expected initial goal from sorry"

  let snapshot3 ← snapshot2.tactic "constructor"
  if snapshot3.rootGoals.length != 2 then
    return TestResult.failure "Complex theorem" s!"Expected 2 goals after constructor, got {snapshot3.rootGoals.length}"

  let snapshot4 ← snapshot3.tactic "exact hp"
  if snapshot4.rootGoals.length != 1 then
    return TestResult.failure "Complex theorem" s!"Expected 1 goal after first exact, got {snapshot4.rootGoals.length}"

  let snapshot5 ← snapshot4.tactic "exact hq"
  if snapshot5.complete? then
    return TestResult.success "Complex theorem"
  else
    return TestResult.failure "Complex theorem" s!"Expected theorem to be complete, but {snapshot5.rootGoals.length} goals remain"

def test_multi_step_arithmetic : IO TestResult := do
  let snapshot ← LeanSnapshot.create

  let snapshot2 ← snapshot.command "theorem test_arith (n : Nat) : n + 0 = n := by sorry"
  if snapshot2.complete? then
    return TestResult.failure "Multi-step arithmetic" "Expected initial goal from sorry"

  let snapshot3 ← snapshot2.tactic "simp"
  if snapshot3.complete? then
    return TestResult.success "Multi-step arithmetic"
  else
    return TestResult.failure "Multi-step arithmetic" "Expected theorem to be complete after simp"

def test_rewrite_steps : IO TestResult := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test_rw (a b c : Nat) (h1 : a = b) (h2 : b = c) : a = c := by sorry"

  if snapshot2.complete? then
    return TestResult.failure "Rewrite steps" "Expected initial goal from sorry"

  let snapshot3 ← snapshot2.tactic "rw [h1]"
  if snapshot3.rootGoals.length != 1 then
    return TestResult.failure "Rewrite steps" s!"Expected 1 goal after first rewrite, got {snapshot3.rootGoals.length}"

  let snapshot4 ← snapshot3.tactic "rw [h2]"
  if snapshot4.complete? then
    return TestResult.success "Rewrite steps"
  else
    return TestResult.failure "Rewrite steps" "Expected theorem to be complete after second rewrite"

def run: IO Bool := do
  runTests "Snapshot Type Serialization" [
    test_empty_snapshot,
    test_env_snapshot,
    test_lean_context,
    test_command_with_sorry,
    test_tactic_application,
    test_goals_formatting,
    test_multiple_goals,
    test_complex_theorem,
    test_multi_step_arithmetic,
    test_rewrite_steps
  ]

end SnapshotTests
