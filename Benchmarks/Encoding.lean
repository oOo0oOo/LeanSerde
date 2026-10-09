import LeanSerde.Core
open LeanSerde
partial def chain : Nat → SerialValue
  | 0 => .nat 0
  | n+1 => .compound "node" #[chain n]
instance : Serializable SerialValue where
  encode x := pure x
  decode x := pure x
def main : IO Unit := do
  for n in [100, 200, 400, 800] do
    let x := chain n
    let t ← IO.monoMsNow
    for _ in [:10] do
      let g ← encodeGraph x
      if g.objects.size > 0 then throw (IO.userError "unexpected sharing")
    IO.println s!"depth={n} repeats=10 elapsed_ms={(← IO.monoMsNow)-t}"
    (← IO.getStdout).flush
