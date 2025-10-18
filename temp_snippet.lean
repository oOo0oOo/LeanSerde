import Lean
import LeanSerde
import LeanSerde.SnapshotTypes

open LeanSerde

def testDirectTactic : IO Unit := do
  let snapshot ← LeanSnapshot.create
  let snapshot2 ← snapshot.command "theorem test_arith (n : Nat) : n + 0 = n := by sorry"
  
  let snapshot3 ← snapshot2.tactic "apply Nat.add_zero"
  IO.println s!"After apply: complete={snapshot3.complete?}, goals={snapshot3.rootGoals.length}"

#eval! testDirectTactic