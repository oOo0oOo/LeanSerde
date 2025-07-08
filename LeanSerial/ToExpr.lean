import Lean

namespace Lean.Serialization

/--
A typeclass for converting a runtime value of type `α` into a `Lean.Expr`.
This is the core mechanism for serialization. We convert the value into an expression,
and then use Lean's built-in machinery to serialize that expression.
-/
class ToExpr (α : Type) where
  /-- Converts `a : α` to a `Lean.Expr`. -/
  toExpr : α → CoreM Expr

export ToExpr (toExpr)

--== Primitive Instances ==--

instance : ToExpr String where
  toExpr s := return mkStrLit s

instance : ToExpr Nat where
  toExpr n := return mkNatLit n

instance : ToExpr Bool where
  toExpr
    | true  => return mkConst ``true
    | false => return mkConst ``false

/--
Instance for `Int`.
Positive integers are represented as `Int.ofNat n`.
Negative integers are represented as `Int.negSucc n`, where the value is `-(n+1)`.
-/
instance : ToExpr Int where
  toExpr i :=
    if i ≥ 0 then
      let n := i.toNat
      return mkApp (mkConst ``Int.ofNat) (mkNatLit n)
    else
      let n := (-i - 1).toNat
      return mkApp (mkConst ``Int.negSucc) (mkNatLit n)

--== Container Instances ==--

/-- Instance for `Option`. Requires an instance for the inner type. -/
instance [ToExpr α] : ToExpr (Option α) where
  toExpr
    | none   => return mkConst ``Option.none
    | some a => return mkApp (mkConst ``Option.some) (← toExpr a)

/-- Instance for `List`. Requires an instance for the inner type. -/
instance [ToExpr α] : ToExpr (List α) where
  toExpr xs := do
    let exprs ← xs.mapM toExpr  -- Remove the 'mut' keyword
    let nil := mkConst ``List.nil
    let cons := mkConst ``List.cons
    return exprs.foldr (fun x acc => mkApp (mkApp cons x) acc) nil

/-- Instance for `Array`. Requires an instance for the inner type. -/
instance [ToExpr α] : ToExpr (Array α) where
  toExpr xs := do
    let listExpr ← toExpr xs.toList
    return mkApp (mkConst ``List.toArray) listExpr

end Lean.Serialization
