import Lean
import Lean.Meta
import Lean.Elab
import Lean.Parser
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.ToExpr
import LeanSerial.FromExpr

open Lean Elab Meta Term Command

-- Define the Serialize class
class Serialize (α : Type) extends Lean.Serialization.ToExpr α, Lean.Serialization.FromExpr α

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

def mkSerializeInstance (typeName : Name) : CommandElabM Unit := do
  -- Generate ToExpr and FromExpr instances
  mkToExprInstance typeName
  mkFromExprInstance typeName

  -- Generate the Serialize instance
  let typeId := mkIdent typeName
  let cmd ← `(instance : Serialize $typeId := {})
  elabCommand cmd

def mkSerializeInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  if (← declNames.allM fun name => do
    let env ← getEnv
    return getStructureInfo? env name |>.isSome) then
    for declName in declNames do
      mkSerializeInstance declName
    return true
  else
    return false

initialize
  registerDerivingHandler `Serialize mkSerializeInstanceHandler
