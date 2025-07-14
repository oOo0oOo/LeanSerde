import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.Core

open Lean Elab Meta Term Command

structure ConstructorData where
  encodePattern : TSyntax `term
  encodeElems : Array (TSyntax `term)
  name : String
  ctorApp : TSyntax `term
  decodeStmts : Array (TSyntax `doElem)

private def mkFieldName (i : Nat) : Name := Name.mkSimple s!"field{i}"

private def mkAuxFunctionName (name: String) (typeId : TSyntax `ident) : Ident :=
  mkIdent (Name.mkSimple s!"{name}_impl_{typeId}")

private def generateContainerEncode (fieldType : Expr) (fieldTerm : TSyntax `term) (encodeFnName : Ident) : CommandElabM (TSyntax `term) := do
  match fieldType with
  | .app (.const `List _) _ =>
    `(LeanSerial.SerialValue.compound "List" (($fieldTerm).map $encodeFnName:ident |>.toArray))
  | .app (.const `Array _) _ =>
    `(LeanSerial.SerialValue.compound "Array" (($fieldTerm).map $encodeFnName:ident))
  | _ => throwError "Invalid container type"

private def generateContainerDecode (fieldType : Expr) (fieldId : Ident) (index : Nat) (decodeFnName : Ident) : CommandElabM (List (TSyntax `doElem)) := do
  match fieldType with
  | .app (.const `List _) _ => pure [
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let containerArgs ← LeanSerial.decodeCompound "List" containerSv),
      ← `(doElem| let listResult ← containerArgs.mapM $decodeFnName:ident),
      ← `(doElem| let $fieldId := listResult.toList)
    ]
  | .app (.const `Array _) _ => pure [
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let containerArgs ← LeanSerial.decodeCompound "Array" containerSv),
      ← `(doElem| let $fieldId ← containerArgs.mapM $decodeFnName:ident)
    ]
  | _ => throwError "Invalid container type"

private def mkConstructorData (typeId : TSyntax `ident) (inductVal : InductiveVal) (ctor : ConstructorVal) : CommandElabM ConstructorData := do
  let ctorId := mkIdent ctor.name
  let fieldIds := (Array.range ctor.numFields).map (mkIdent ∘ mkFieldName)
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
      return types

  -- Helper function to check if a type contains the inductive type being defined
  let rec containsInductiveType (expr : Expr) : Bool :=
    if expr.isAppOf inductVal.name then
      true
    else
      match expr with
      | .app f a => containsInductiveType f || containsInductiveType a
      | _ => false

  -- Helper function to check if this is a direct container of the inductive type
  let isDirectContainer (expr : Expr) : Bool :=
    match expr with
    | .app (.const name _) inner =>
      (name == `List || name == `Array) && inner.isAppOf inductVal.name
    | _ => false

  let result ← fieldTerms.zip fieldIds |>.zip fieldTypes |>.mapIdxM fun i ((fieldTerm, fieldId), fieldType) => do
    let encodeElem ← if fieldType.isAppOf inductVal.name then
      `($encodeFnName:ident $fieldTerm)
    else if isDirectContainer fieldType then
      generateContainerEncode fieldType fieldTerm encodeFnName
    else
      `(LeanSerial.encode $fieldTerm)

    let decodeStmt ← if fieldType.isAppOf inductVal.name then
      pure #[← `(doElem| let $fieldId ← $decodeFnName:ident args[$(quote i)]!)]
    else if isDirectContainer fieldType then
      let stmts ← generateContainerDecode fieldType fieldId i decodeFnName
      pure stmts.toArray
    else
      pure #[← `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)]

    return (encodeElem, decodeStmt)

  let (encodeElems, decodeStmtsList) := result.unzip
  let decodeStmts := decodeStmtsList.flatten

  let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

  let encodePattern ←
    if ctor.numFields = 0 then
      pure ⟨ctorId⟩
    else
      `($(ctorId) $fieldTerms*)

  return {
    encodePattern := encodePattern,
    encodeElems := encodeElems,
    name := ctor.name.toString,
    ctorApp := ctorApp,
    decodeStmts := decodeStmts
  }

private def mkSerializableQuotation (typeId : TSyntax `ident) (constructorData : Array ConstructorData) (constructorInfos : Array ConstructorVal) (isRecursive : Bool) : CommandElabM (Array (TSyntax `command)) := do
  let encodeMatches := constructorData.map fun cd =>
    (cd.encodePattern, cd.encodeElems, cd.name)

  let encodeArms ← encodeMatches.mapM fun (_, elems, name) =>
    `(LeanSerial.SerialValue.compound $(quote name) #[$(elems),*])

  let decodeArms ← constructorData.mapIdxM fun i data => do
    let numFields := constructorInfos[i]!.numFields
    `(doSeq|
      if args.size = $(quote numFields) then do
        $[$(data.decodeStmts):doElem]*
        .ok $(data.ctorApp)
      else
        .error "Field count mismatch")

  let decodePatterns ← constructorData.mapM fun data => `($(quote data.name))
  let encodePatterns := constructorData.map (·.encodePattern)

  let encodeFnName := mkAuxFunctionName "encode" typeId
  let decodeFnName := mkAuxFunctionName "decode" typeId

  let encodeDef ← if isRecursive then
    `(partial def $encodeFnName (v : $typeId) : LeanSerial.SerialValue :=
        match v with
        $[| $encodePatterns => $encodeArms]*)
  else
    `(def $encodeFnName (v : $typeId) : LeanSerial.SerialValue :=
        match v with
        $[| $encodePatterns => $encodeArms]*)

  let decodeDef ← if isRecursive then
    `(partial def $decodeFnName (sv : LeanSerial.SerialValue) : Except String $typeId := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        $[| $decodePatterns => $decodeArms]*
        | _ => .error "Unknown constructor")
  else
    `(def $decodeFnName (sv : LeanSerial.SerialValue) : Except String $typeId := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        $[| $decodePatterns => $decodeArms]*
        | _ => .error "Unknown constructor")

  let inst ← `(instance : LeanSerial.Serializable $typeId where
      encode := $encodeFnName
      decode := $decodeFnName)

  return #[encodeDef, decodeDef, inst]

def mkSerializableInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some (ConstantInfo.inductInfo inductVal) := env.find? typeName
    | throwError s!"Type {typeName} is not an inductive type"

  let typeId := mkIdent typeName
  let constructorInfosArray ← inductVal.ctors.toArray.mapM fun ctorName => do
    let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName
      | throwError s!"constructor {ctorName} not found"
    return ctorVal

  if constructorInfosArray.isEmpty then
    throwError "Empty inductive type"

  let constructorData ← constructorInfosArray.mapM (mkConstructorData typeId inductVal)
  let cmds ← mkSerializableQuotation typeId constructorData constructorInfosArray inductVal.isRec

  cmds.forM elabCommand

initialize
  registerDerivingHandler ``LeanSerial.Serializable fun declNames => do
    for declName in declNames do
      mkSerializableInstance declName
    return true
