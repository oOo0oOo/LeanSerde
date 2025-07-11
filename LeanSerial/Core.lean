import Lean
import LeanSerial.Derive

open Lean Elab Meta Term Command

namespace LeanSerial

private def exprToString (expr : Expr) : CoreM String := do
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
    let fmt ← MetaM.run' (PrettyPrinter.ppExpr expr)
    return toString fmt

private def stringToExpr (exprStr : String) : CoreM Expr := do
  -- Simple s-expression parser (flat, not nested)
  let inner := exprStr.drop 1 |>.dropRight 1
  let parts := inner.splitOn " "
  let ctorNameStr := parts.head!
  let argsStr := parts.tail!
  let ctorName := ctorNameStr.toName
  let argExprs ← argsStr.mapM fun argStr =>
    if argStr.startsWith "\"" then
      pure <| mkStrLit (argStr.drop 1 |>.dropRight 1)
    else if let some n := argStr.toNat? then
      pure <| mkNatLit n
    else
      throwError s!"Unsupported argument type for deserialization: {argStr}"
  let ctor := Lean.mkConst ctorName
  pure <| mkAppN ctor argExprs.toArray

def serializeCore {α} [Serialization.ToExpr α] (a : α) : CoreM ByteArray := do
  let expr ← Serialization.toExpr a
  let exprStr ← exprToString expr
  return exprStr.toUTF8

def deserializeCore {α} [Serialization.FromExpr α] (bytes : ByteArray) : CoreM α := do
  let exprStr := String.fromUTF8! bytes
  let expr ← stringToExpr exprStr
  Serialization.fromExpr expr

end LeanSerial
