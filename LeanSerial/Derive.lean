import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.Serializable

open Lean Elab Meta Term Command

private def mkFieldName (i : Nat) : Name := Name.mkSimple s!"field{i}"

private def mkAuxFunctionName (name: String) (typeId : TSyntax `ident) : Ident :=
  mkIdent (Name.mkSimple s!"{name}_impl_{typeId}")

private def mkConstructorData (typeId : TSyntax `ident) (inductVal : InductiveVal) (ctor : ConstructorVal) : CommandElabM (TSyntax `term × List (TSyntax `term) × String × TSyntax `term × List (TSyntax `doElem)) := do
  let ctorId := mkIdent ctor.name
  let fieldIds := (List.range ctor.numFields).map (mkIdent ∘ mkFieldName)
  let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

  let encodeFnName := mkAuxFunctionName "encode" typeId
  let decodeFnName := mkAuxFunctionName "decode" typeId

  let ctorInfo ← getConstInfoCtor ctor.name
  let fieldTypes ← liftTermElabM do
    forallTelescopeReducing ctorInfo.type fun xs _ => do
      let mut types : Array Expr := #[]
      for i in [:ctor.numFields] do
        let x := xs[inductVal.numParams + i]!
        let localDecl ← x.fvarId!.getDecl
        types := types.push localDecl.type
      return types.toList

  let encodeElems ← fieldTerms.zip fieldTypes |>.mapM fun (fieldTerm, fieldType) => do
    if fieldType.isAppOf inductVal.name then
      `($encodeFnName:ident $fieldTerm)
    else
      `(LeanSerial.encode $fieldTerm)

  let decodeStmts ← fieldIds.zip fieldTypes |>.mapIdxM fun i (fieldId, fieldType) => do
    if fieldType.isAppOf inductVal.name then
      `(doElem| let $fieldId ← $decodeFnName:ident args[$(quote i)]!)
    else
      `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

  let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

  let encodePattern ←
    if ctor.numFields = 0 then
      pure ⟨ctorId⟩
    else
      let fieldArray := fieldTerms.toArray
      `($(ctorId) $fieldArray*)

  return (encodePattern, encodeElems, ctor.name.toString, ctorApp, decodeStmts)

private def mkSerializableQuotation (typeId : TSyntax `ident) (constructorData : List (TSyntax `term × List (TSyntax `term) × String × TSyntax `term × List (TSyntax `doElem))) (constructorInfos : List ConstructorVal) (isRecursive : Bool) : CommandElabM (Array (TSyntax `command)) := do
  let encodeArms ← constructorData.mapM fun (_, elems, name, _, _) => do
    `(LeanSerial.SerialValue.compound $(quote name) #[$(elems.toArray),*])

  let decodeArms ← constructorData.mapIdxM fun i (_, _, _, ctorApp, decodeStmts) => do
    let numFields := constructorInfos[i]!.numFields
    `(doSeq|
      if args.size = $(quote numFields) then do
        $[$(decodeStmts.toArray):doElem]*
        .ok $ctorApp
      else
        .error "Field count mismatch")

  let encodeArms := encodeArms.toArray
  let decodeArms := decodeArms.toArray

  let decodePatterns ← constructorData.mapM fun (_, _, name, _, _) => `($(quote name))
  let encodePatterns := constructorData.map (·.1)

  let encodeFnName := mkAuxFunctionName "encode" typeId
  let decodeFnName := mkAuxFunctionName "decode" typeId

  let encodeDef ← if isRecursive then
    `(partial def $encodeFnName (v : $typeId) : LeanSerial.SerialValue :=
        match v with
        $[| $(encodePatterns.toArray) => $(encodeArms)]*)
  else
    `(def $encodeFnName (v : $typeId) : LeanSerial.SerialValue :=
        match v with
        $[| $(encodePatterns.toArray) => $(encodeArms)]*)

  let decodeDef ← if isRecursive then
    `(partial def $decodeFnName (sv : LeanSerial.SerialValue) : Except String $typeId := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        $[| $(decodePatterns.toArray) => $(decodeArms)]*
        | _ => .error "Unknown constructor")
  else
    `(def $decodeFnName (sv : LeanSerial.SerialValue) : Except String $typeId := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        $[| $(decodePatterns.toArray) => $(decodeArms)]*
        | _ => .error "Unknown constructor")

  let inst ← `(instance : LeanSerial.Serializable $typeId where
      encode := $encodeFnName
      decode := $decodeFnName)

  return #[encodeDef, decodeDef, inst]

def mkSerializableInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some (ConstantInfo.inductInfo inductVal) := env.find? typeName
    | throwError "not an inductive type"

  let typeId := mkIdent typeName
  let constructorInfos ← inductVal.ctors.mapM fun ctorName => do
    let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName
      | throwError s!"constructor {ctorName} not found"
    return ctorVal

  if constructorInfos.isEmpty then
    throwError "Empty inductive type"

  let constructorData ← constructorInfos.mapM (mkConstructorData typeId inductVal)
  let cmds ← mkSerializableQuotation typeId constructorData constructorInfos inductVal.isRec

  cmds.forM elabCommand

def mkSerializableInstanceHandler (declName : Name) : CommandElabM Bool := do
  let env ← getEnv
  let isInductive := env.find? declName matches some (ConstantInfo.inductInfo _)
  let hasConstructor := env.find? (declName ++ `mk) matches some (ConstantInfo.ctorInfo _)

  if isInductive || hasConstructor then do
    mkSerializableInstance declName
    return true
  else
    return false

initialize
  registerDerivingHandler ``LeanSerial.Serializable fun declNames => do
    for declName in declNames do
      unless ← mkSerializableInstanceHandler declName do
        throwError "Failed to generate Serializable instance for {declName}"
    return true
