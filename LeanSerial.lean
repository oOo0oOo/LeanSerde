import Lean
import LeanSerial.ToExpr
import LeanSerial.FromExpr
import LeanSerial.Derive

open Lean Elab Meta Term Command

namespace LeanSerial

private def exprToString (expr : Expr) : CoreM String := do
  -- Use a custom string representation that preserves constructor applications
  match expr with
  | .app (.app (.const name _) arg1) arg2 => do
    let arg1Str ← exprToString arg1
    let arg2Str ← exprToString arg2
    return s!"({name} {arg1Str} {arg2Str})"
  | .app (.const name _) arg => do
    let argStr ← exprToString arg
    return s!"({name} {argStr})"
  | .const name _ => return s!"{name}"
  | .lit (.strVal s) => return s!"\"{s}\""
  | .lit (.natVal n) => return s!"{n}"
  | _ =>
    -- Fallback to pretty printer for other cases
    let fmt ← MetaM.run' (PrettyPrinter.ppExpr expr)
    return toString fmt

private def serializeCore {α} [Serialization.ToExpr α] (a : α) : CoreM ByteArray := do
  let expr ← Serialization.toExpr a
  let exprStr ← exprToString expr
  return exprStr.toUTF8

private def deserializeCore {α} [Serialization.FromExpr α] (bytes : ByteArray) : CoreM α := do
  let exprStr := String.fromUTF8! bytes
  logInfo s!"Deserializing string: {exprStr}"
  let env ← getEnv
  let parserResult := Parser.runParserCategory env `term exprStr "<serialized>"

  match parserResult with
  | .error err =>
    throwError s!"Failed to parse serialized expression: {err}"
  | .ok termStx => do
    logInfo s!"Parsed syntax: {termStx}"
    let expr ← MetaM.run' (do
      let result ← TermElabM.run' (do
        let expr ← Term.elabTerm termStx none
        Term.synthesizeSyntheticMVars
        return expr
      ) {}
      pure result
    )
    logInfo s!"Elaborated expression: {expr}"
    Serialization.fromExpr expr

-- Simplified IO versions that create a minimal but working environment
def serialize {α} [Serialization.ToExpr α] (a : α) : IO (Except String ByteArray) := do
  try
    -- Create an environment with the current module loaded
    let env ← importModules #[{module := `LeanSerial}] {}
    let (bytes, _) ← (serializeCore a).toIO { fileName := "<serialize>", fileMap := default } { env := env, ngen := default }
    pure (.ok bytes)
  catch e =>
    pure (.error (toString e))

def deserialize {α} [Serialization.FromExpr α] (bytes : ByteArray) : IO (Except String α) := do
  try
    -- Create an environment with the current module loaded
    let env ← importModules #[{module := `LeanSerial}] {}
    let (a, _) ← (deserializeCore bytes).toIO { fileName := "<deserialize>", fileMap := default } { env := env, ngen := default }
    pure (.ok a)
  catch e =>
    pure (.error (toString e))

end LeanSerial
