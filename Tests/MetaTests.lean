import Lean
import Lean.Elab
import Lean.Meta

import Tests.TestFramework
import LeanSerial


open TestFramework Lean Elab Meta Term Command

namespace MetaTests

def test_name : IO TestResult := do
  let name := Name.mkSimple "test.name"
  test_roundtrip "Name" name

def test_lmvar_id : IO TestResult := do
  let lmvarId : LMVarId := { name := Name.mkSimple "test.lmvar" }
  test_roundtrip "LMVarId" lmvarId

def test_level : IO TestResult := do
  let level : Level := Level.param (Name.mkSimple "test.level")
  test_roundtrip "Level" level

def test_sourceinfo : IO TestResult := do
  let sourceInfo := SourceInfo.synthetic ⟨10⟩ ⟨20⟩ false
  test_roundtrip "SourceInfo" sourceInfo

def test_syntax : IO TestResult := do
  let syn := Syntax.node SourceInfo.none `testNode #[Syntax.atom SourceInfo.none "test"]
  test_roundtrip "Syntax" syn

def test_expr : IO TestResult := do
  let expr := Expr.const (Name.mkSimple "testExpr") []
  test_roundtrip "Expr" expr

def test_complex_expr : IO TestResult := do
  let expr := Expr.app (Expr.const (Name.mkSimple "testFunc") []) (Expr.lit (Literal.strVal "testArg"))
  test_roundtrip "Expr App" expr

def real_life_expr : IO TestResult := do
  -- This represents: (fun (α : Type) (x : α) (h : x = x) => Eq.refl x) Nat 42 (Eq.refl 42)
  let typeSort := Expr.sort Level.zero
  let natType := Expr.const `Nat []
  let natVal := Expr.lit (Literal.natVal 42)
  let alphaParam := Expr.fvar ⟨Name.mkSimple "α"⟩
  let xParam := Expr.fvar ⟨Name.mkSimple "x"⟩
  let eqType := Expr.app (Expr.app (Expr.app (Expr.const `Eq [Level.zero]) alphaParam) xParam) xParam
  let eqReflConst := Expr.const `Eq.refl [Level.zero]
  let eqReflApp := Expr.app (Expr.app eqReflConst alphaParam) xParam
  let innerLam := Expr.lam `h eqType eqReflApp BinderInfo.default
  let middleLam := Expr.lam `x alphaParam innerLam BinderInfo.default
  let outerLam := Expr.lam `α typeSort middleLam BinderInfo.default
  let natRefl := Expr.app (Expr.app (Expr.const `Eq.refl [Level.zero]) natType) natVal
  let secondApp := Expr.app (Expr.app outerLam natType) natVal
  let finalExpr := Expr.app secondApp natRefl
  test_roundtrip "Real-Life Expr" finalExpr


def run : IO Unit := do
  runTests "Meta Type Serialization" [
    test_name,
    test_lmvar_id,
    test_level,
    test_sourceinfo,
    test_syntax,
    test_expr,
    test_complex_expr,
    real_life_expr
  ]

end MetaTests
