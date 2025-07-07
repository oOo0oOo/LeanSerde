import Lean
import Lean.Meta
import Lean.Elab
import Lean.Parser
import Lean.Elab.Command
import LeanSerial.ToExpr
import LeanSerial.FromExpr

open Lean Elab Meta Term Command

def mkToExprInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some info := getStructureInfo? env typeName | throwError "not a structure"
  let typeId := mkIdent typeName
  let fieldIds := info.fieldNames.map mkIdent
  -- For structures, use the constructor name (typeName.mk)
  let constructorName := typeName ++ `mk
  let cmd ← `(
    instance : Lean.Serialization.ToExpr $typeId where
      toExpr v := do
        let fieldExprs ← #[ $[(Lean.Serialization.toExpr v.$(fieldIds):ident)],* ].mapM id
        return mkAppN (mkConst $(quote constructorName)) fieldExprs
  )
  elabCommand cmd

def mkFromExprInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some info := getStructureInfo? env typeName | throwError "not a structure"
  let typeId := mkIdent typeName
  let fieldIds := info.fieldNames.map mkIdent
  let fieldCount := info.fieldNames.size
  -- For structures, use the constructor name (typeName.mk)
  let constructorName := typeName ++ `mk

  if fieldCount == 2 then
    let cmd ← `(
      instance : Lean.Serialization.FromExpr $typeId where
        fromExpr e := do
          -- Check if this is the expected constructor
          if !e.isAppOfArity $(quote constructorName) $(quote fieldCount) then
            throwError s!"expected {$(quote constructorName)} with {$(quote fieldCount)} arguments, got {e}"
          let args := e.getAppArgs
          let field0 ← Lean.Serialization.fromExpr args[0]!
          let field1 ← Lean.Serialization.fromExpr args[1]!
          return { $(fieldIds[0]!):ident := field0, $(fieldIds[1]!):ident := field1 }
    )
    elabCommand cmd
  else
    throwError s!"FromExpr derivation currently only supports structures with exactly 2 fields, got {fieldCount}"

elab "derive_ToExpr " id:ident : command => do
  let typeName := id.getId
  mkToExprInstance typeName

elab "derive_FromExpr " id:ident : command => do
  let typeName := id.getId
  mkFromExprInstance typeName
