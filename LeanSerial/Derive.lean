import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.Serializable

open Lean Elab Meta Term Command

def mkSerializableInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some (ConstantInfo.inductInfo inductVal) := env.find? typeName | throwError "not an inductive type"

  let typeId := mkIdent typeName

  let constructorInfos ← inductVal.ctors.mapM fun ctorName => do
    let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName | throwError s!"constructor {ctorName} not found"
    return ctorVal

  if constructorInfos.isEmpty then
    throwError "Empty inductive type"

  let mkConstructorData (ctor : ConstructorVal) : CommandElabM (TSyntax `term × Array (TSyntax `term) × String × TSyntax `term × Array (TSyntax `doElem)) := do
    let ctorId := mkIdent ctor.name
    let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
    let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

    let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
    let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
      `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

    let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

    let encodePattern ← if isStructure env typeName then
      if ctor.numFields = 0 then
        pure ⟨ctorId⟩
      else
        let fieldArray := fieldTerms.toArray
        `($(ctorId) $fieldArray*)
    else
      if ctor.numFields = 0 then
        pure ⟨ctorId⟩
      else
        let fieldArray := fieldTerms.toArray
        `($(ctorId) $fieldArray*)

    return (encodePattern, encodeElems.toArray, ctor.name.toString, ctorApp, decodeStmts.toArray)

  let constructorData ← constructorInfos.mapM mkConstructorData

  let encodeArmsList ← constructorData.mapM fun (_, elems, name, _, _) => do
    `(LeanSerial.SerialValue.compound $(quote name) #[$elems,*])
  let encodeArms := encodeArmsList.toArray

  let decodeArmsList ← constructorData.mapIdxM fun i (_, _, _, ctorApp, decodeStmts) => do
    let numFields := constructorInfos[i]!.numFields
    `(doSeq|
      if args.size = $(quote numFields) then do
        $[$decodeStmts:doElem]*
        .ok $ctorApp
      else
        .error "Field count mismatch")
  let decodeArms := decodeArmsList.toArray

  let decodePatternsList ← constructorData.mapM fun (_, _, name, _, _) => `($(quote name))
  let decodePatterns := decodePatternsList.toArray

  let encodePatterns := (constructorData.map (·.1)).toArray

  let cmd ← `(instance : LeanSerial.Serializable $typeId where
      encode v := match v with
        $[| $(encodePatterns) => $(encodeArms)]*
      decode sv := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        $[| $(decodePatterns) => $(decodeArms)]*
        | _ => .error "Unknown constructor")
  elabCommand cmd

def mkSerializableInstanceHandler (declName : Name) : CommandElabM Bool := do
  let env ← getEnv
  match env.find? declName with
  | some (ConstantInfo.inductInfo _) =>
    mkSerializableInstance declName
    return true
  | _ =>
    match env.find? (declName ++ `mk) with
    | some (ConstantInfo.ctorInfo __) =>
      match env.find? declName with
      | some (ConstantInfo.inductInfo _) =>
        mkSerializableInstance declName
        return true
      | _ => return false
    | _ => return false

initialize
  registerDerivingHandler ``LeanSerial.Serializable fun declNames => do
    for declName in declNames do
      unless ← mkSerializableInstanceHandler declName do
        throwError "Failed to generate Serializable instance for {declName}"
    return true
