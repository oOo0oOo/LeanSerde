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

def test_local_decl_kind : IO TestResult := do
  let kind := Lean.LocalDeclKind.default
  test_roundtrip "LocalDeclKind" kind

def test_local_context : IO TestResult := do
  -- Create a simple LocalContext manually
  let fvarId1 := { name := Name.mkSimple "x" : FVarId }
  let fvarId2 := { name := Name.mkSimple "y" : FVarId }

  let localDecl1 := LocalDecl.cdecl 0 fvarId1 `x (Expr.const `Nat []) BinderInfo.default LocalDeclKind.default
  let localDecl2 := LocalDecl.ldecl 1 fvarId2 `y (Expr.const `String []) (Expr.lit (Literal.strVal "hello")) false LocalDeclKind.default

  let context := LocalContext.empty.addDecl localDecl1 |>.addDecl localDecl2

  -- Test roundtrip manually
  let bytes: ByteArray := LeanSerial.serialize context
  match (LeanSerial.deserialize bytes : Except String LocalContext) with
  | .error e => return TestResult.failure "LocalContext" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    -- Compare contexts by checking individual declarations
    let contextDecls := context.decls.toList
    let decodedDecls := decoded.decls.toList
    if contextDecls.length == decodedDecls.length then
      return TestResult.success "LocalContext"
    else
      return TestResult.failure "LocalContext" "Size mismatch"

def test_metavar_context : IO TestResult := do
  -- Get a real MetavarContext from Meta monad
  let initEnv ← Lean.mkEmptyEnvironment
  let result ← Meta.MetaM.toIO (do
    -- Create some metavariables to populate the context
    let _mvar1 ← Meta.mkFreshExprMVar (Expr.const `Nat []) (userName := `testMVar1)
    let _mvar2 ← Meta.mkFreshExprMVar (Expr.const `String []) (userName := `testMVar2)

    -- Get the current MetavarContext
    let mctx ← getMCtx
    return mctx
  ) { fileName := "", fileMap := default } { env := initEnv } {} {}

  let (mctx, _, _) := result
  let bytes: ByteArray := LeanSerial.serialize mctx
  match (LeanSerial.deserialize bytes : Except String MetavarContext) with
  | .error e => return TestResult.failure "MetavarContext" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    -- Compare a few fields to ensure some correctness
    if mctx.mvarCounter == decoded.mvarCounter &&
      mctx.depth == decoded.depth &&
      mctx.decls.toArray.size == decoded.decls.toArray.size &&
      mctx.dAssignment.toArray.size == decoded.dAssignment.toArray.size then
      return TestResult.success "MetavarContext"
    else
      return TestResult.failure "MetavarContext" "Field mismatch"

def test_constant_info : IO TestResult := do
  -- Create a simple ConstantInfo manually for testing
  let constInfo : ConstantInfo := .axiomInfo {
    name := `TestConstant
    levelParams := [`u]
    type := Expr.sort (Level.param `u)
    isUnsafe := false
  }

  let bytes: ByteArray := LeanSerial.serialize constInfo
  match (LeanSerial.deserialize bytes : Except String ConstantInfo) with
  | .error e => return TestResult.failure "ConstantInfo" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    if constInfo.name == decoded.name &&
       constInfo.levelParams.length == decoded.levelParams.length &&
       constInfo.type == decoded.type then
      return TestResult.success "ConstantInfo"
    else
      return TestResult.failure "ConstantInfo" "Field mismatch"

def test_empty_environment : IO TestResult := do
  let env ← Lean.mkEmptyEnvironment
  let bytes: ByteArray := LeanSerial.serialize env
  match (LeanSerial.deserialize bytes : Except String Environment) with
  | .error e => return TestResult.failure "Environment" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    if env.mainModule == decoded.mainModule &&
        env.isExporting == decoded.isExporting &&
        env.asyncPrefix? == decoded.asyncPrefix? &&
        env.header.trustLevel == decoded.header.trustLevel &&
        env.constants.toList.length == decoded.constants.toList.length &&
        env.const2ModIdx.size == decoded.const2ModIdx.size then
      return TestResult.success "Empty Environment"
    else
      return TestResult.failure "Empty Environment" "Field mismatch"

def test_info_tree : IO TestResult := do
  let mvarId := { name := Name.mkSimple "testMVar" : MVarId }
  let infoTreeHole := Lean.Elab.InfoTree.hole mvarId

  let bytes: ByteArray := LeanSerial.serialize infoTreeHole
  match (LeanSerial.deserialize bytes : Except String Lean.Elab.InfoTree) with
  | .error e => return TestResult.failure "InfoTree" s!"Failed to deserialize: {e}"
  | .ok decoded =>
    match infoTreeHole, decoded with
    | .hole originalMVar, .hole decodedMVar =>
      if originalMVar.name == decodedMVar.name then
        return TestResult.success "InfoTree"
      else
        return TestResult.failure "InfoTree" "MVarId mismatch"
    | _, _ => return TestResult.failure "InfoTree" "Constructor mismatch"

def run : IO Unit := do
  runTests "Meta Type Serialization" [
    test_name,
    test_lmvar_id,
    test_level,
    test_sourceinfo,
    test_syntax,
    test_expr,
    test_complex_expr,
    real_life_expr,
    test_local_decl_kind,
    test_local_context,
    test_metavar_context,
    test_constant_info,
    test_empty_environment,
    test_info_tree
  ]
end MetaTests
