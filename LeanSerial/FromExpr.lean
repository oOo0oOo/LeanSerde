import Lean

namespace Lean.Serialization

/--
A typeclass for converting a `Lean.Expr` into a runtime value of type `α`.
This is the core mechanism for deserialization. We deserialize an expression
from a file, and then use this typeclass to convert it into a runtime value.
-/
class FromExpr (α : Type) where
  /-- Converts a `Lean.Expr` to a value of type `α`. -/
  fromExpr : Expr → CoreM α

export FromExpr (fromExpr)

--== Primitive Instances ==--

instance : FromExpr String where
  fromExpr e :=
    match e with
    | .lit (.strVal s) => return s
    | _ => throwError "expected a string literal, got {e}"

instance : FromExpr Nat where
  fromExpr e :=
    match e with
    | .lit (.natVal n) => return n
    | _ =>
      -- Handle OfNat.ofNat applications that result from elaboration
      if e.isAppOfArity ``OfNat.ofNat 3 then
        let args := e.getAppArgs
        -- The second argument should be the literal
        match args[1]! with
        | .lit (.natVal n) => return n
        | _ => throwError "expected a natural number literal in OfNat.ofNat, got {args[1]!}"
      else
        throwError "expected a natural number literal, got {e}"

instance : FromExpr Bool where
  fromExpr e := do
    let constName ←
      if e.isConst then
        pure e.constName!
      else
        throwError "expected a constant, got {e}"
    if constName == ``true then
      return true
    else if constName == ``false then
      return false
    else
      throwError "expected true or false, got {constName}"

/--
Instance for `Int`.
Positive integers are represented as `Int.ofNat n`.
Negative integers are represented as `Int.negSucc n`, where the value is `-(n+1)`.
-/
instance : FromExpr Int where
  fromExpr e := do
    if e.isAppOfArity ``Int.ofNat 1 then
      let n ← fromExpr e.appArg!
      return Int.ofNat n
    else if e.isAppOfArity ``Int.negSucc 1 then
      let n ← fromExpr e.appArg!
      return Int.negSucc n
    else
      throwError "expected an Int, got {e}"

--== Container Instances ==--

/-- Instance for `Option`. -/
instance [FromExpr α] : FromExpr (Option α) where
  fromExpr e := do
    if e.isAppOfArity ``Option.some 1 then
      let val ← fromExpr e.appArg!
      return some val
    else if e.isConstOf ``Option.none then
      return none
    else
      throwError "expected an Option, got {e}"

/-- Instance for `List`. -/
instance [FromExpr α] : FromExpr (List α) where
  fromExpr e := do
    let mut expr := e
    let mut xs := []
    while expr.isAppOfArity ``List.cons 2 do
      let xExpr := expr.getAppArgs[0]!
      let restExpr := expr.getAppArgs[1]!
      let x ← fromExpr xExpr
      xs := x :: xs
      expr := restExpr
    if expr.isConstOf ``List.nil then
      return xs.reverse
    else
      throwError "expected a List, got {e}"

/-- Instance for `Array`. -/
instance [FromExpr α] : FromExpr (Array α) where
  fromExpr e := do
    if e.isAppOfArity ``List.toArray 1 then
      let listExpr := e.appArg!
      let list : List α ← fromExpr listExpr
      return Array.mk list
    else
      throwError "expected an Array, got {e}"

end Lean.Serialization
