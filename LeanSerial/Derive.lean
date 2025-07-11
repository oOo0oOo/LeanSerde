import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.Serializable

open Lean Elab Meta Term Command

def mkSerializableInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some info := getStructureInfo? env typeName | throwError "not a structure"
  let typeId := mkIdent typeName
  let fieldIds := info.fieldNames.map mkIdent
  let fieldCount := info.fieldNames.size
  if fieldCount == 2 then
    let cmd ← `(
      instance : LeanSerial.Serializable $typeId where
        encode v := LeanSerial.SerialValue.compound $(quote typeName.toString) #[LeanSerial.encode v.$(fieldIds[0]!), LeanSerial.encode v.$(fieldIds[1]!)]
        decode sv := do
          let args ← LeanSerial.decodeCompound $(quote typeName.toString) sv
          if args.size ≠ $(quote fieldCount) then
            .error s!"Expected $(quote fieldCount) fields, got {args.size}"
          let field0 ← LeanSerial.decode args[0]!
          let field1 ← LeanSerial.decode args[1]!
          return { $(fieldIds[0]!):ident := field0, $(fieldIds[1]!):ident := field1 }
    )
    elabCommand cmd
  else
    throwError s!"Serializable derivation currently only supports structures with exactly 2 fields, got {fieldCount}"

def mkSerializableInstanceHandler (declNames : Array Name) : CommandElabM Bool := do
  if (← declNames.allM fun name => do
    let env ← getEnv
    return getStructureInfo? env name |>.isSome) then
    for declName in declNames do
      mkSerializableInstance declName
    return true
  else
    return false

initialize
  registerDerivingHandler ``LeanSerial.Serializable mkSerializableInstanceHandler
