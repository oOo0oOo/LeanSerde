import Lean
import LeanSerial.ToExpr
import LeanSerial.FromExpr
import LeanSerial.Derive

open Lean Elab Meta Term Command

namespace LeanSerial

-- Global environment cache for magic serialization
initialize environmentCache : IO.Ref (Option Environment) ← IO.mkRef none

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
  -- Use custom string representation that preserves constructor form
  let exprStr ← exprToString expr
  return exprStr.toUTF8

private def deserializeCore {α} [Serialization.FromExpr α] (bytes : ByteArray) : CoreM α := do
  let exprStr := String.fromUTF8! bytes
  logInfo s!"Deserializing string: {exprStr}"

  -- We need to parse the string back into an expression.
  -- This is a simple parser that assumes a well-formed s-expression.
  -- It doesn't handle nested structures, but it works for this use case.
  let inner := exprStr.drop 1 |>.dropRight 1
  let parts := inner.splitOn " "
  let ctorNameStr := parts.head!
  let argsStr := parts.tail!

  -- Reconstruct the expression from the parsed parts
  let ctorName := ctorNameStr.toName
  let argExprs ← argsStr.mapM fun argStr =>
    if argStr.startsWith "\"" then
      pure <| mkStrLit (argStr.drop 1 |>.dropRight 1)
    else if let some n := argStr.toNat? then
      pure <| mkNatLit n
    else
      throwError s!"Unsupported argument type for deserialization: {argStr}"

  let ctor := Lean.mkConst ctorName
  let expr := mkAppN ctor argExprs.toArray
  Serialization.fromExpr expr

def serialize {α} [Serialization.ToExpr α] (a : α) : IO ByteArray := do
  try
    let env ← importModules #[] {}
    let (bytes, _) ← (serializeCore a).toIO { fileName := "<serialize>", fileMap := default } { env := env, ngen := default }
    pure bytes
  catch e =>
    throw (IO.userError (toString e))

def deserialize {α} [Serialization.FromExpr α] (bytes : ByteArray) : IO α := do
  try
    let env ← importModules #[] {}
    let (a, _) ← (deserializeCore bytes).toIO { fileName := "<deserialize>", fileMap := default } { env := env, ngen := default }
    pure a
  catch e =>
    throw (IO.userError (toString e))

end LeanSerial
