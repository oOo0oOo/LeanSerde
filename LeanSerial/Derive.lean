import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerial.Serializable

open Lean Elab Meta Term Command


def mkSerializableInstance (ctor : ConstructorVal) (typeName : Name) : CommandElabM Unit := do
  let typeId := mkIdent typeName
  let ctorId := mkIdent ctor.name
  let fieldCount := ctor.numFields

  -- Generate field identifiers
  let fieldIds := (List.range fieldCount).map fun i => mkIdent (Name.mkSimple s!"field{i}")

  -- Convert to TSyntax terms
  let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

  -- Generate encode array elements
  let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)

  -- Generate decode statements with proper indexing
  let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

  -- Generate constructor application with fields
  let ctorTerm : TSyntax `term := ⟨ctorId⟩
  let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ctorTerm

  -- Generate pattern - use anonymous constructor syntax for structures
  let pattern ← if fieldTerms.isEmpty then
    pure ctorTerm
  else
    -- For structures, use ⟨field0, field1, ...⟩ pattern
    let fieldArray := fieldTerms.toArray
    `(⟨$fieldArray,*⟩)

  -- Convert encode elements to proper syntax
  let encodeArray := encodeElems.toArray

  let cmd ← `(
    instance : LeanSerial.Serializable $typeId where
      encode v := match v with
        | $pattern => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$encodeArray,*]
      decode sv := do
        let args ← LeanSerial.decodeCompound $(quote ctor.name.toString) sv
        if args.size = $(quote fieldCount) then do
          $[$(decodeStmts.toArray):doElem]*
          .ok $ctorApp
        else
          .error "Field count mismatch"
  )
  elabCommand cmd


def mkSingleConstructorInstance (ctor : ConstructorVal) (typeId : TSyntax `ident) : CommandElabM Unit := do
  let ctorId := mkIdent ctor.name
  let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

  let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)

  let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

  let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

  let encodePattern ← if ctor.numFields = 0 then
    pure ⟨ctorId⟩
  else
    let fieldArray := fieldTerms.toArray
    pure ⟨← `($(ctorId) $fieldArray*)⟩

  let cmd ← `(
    instance : LeanSerial.Serializable $typeId where
      encode
        | $encodePattern => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]
      decode sv := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        | $(quote ctor.name.toString) => do
          if args.size = $(quote ctor.numFields) then do
            $[$(decodeStmts.toArray):doElem]*
            .ok $ctorApp
          else
            .error "Field count mismatch"
        | _ => .error "Unknown constructor"
  )
  elabCommand cmd


def mkSerializableInstanceForInductive (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some (ConstantInfo.inductInfo inductVal) := env.find? typeName | throwError "not an inductive type"

  let typeId := mkIdent typeName

  let constructorInfos ← inductVal.ctors.mapM fun ctorName => do
    let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName | throwError s!"constructor {ctorName} not found"
    return ctorVal

  if constructorInfos.isEmpty then
    throwError "Empty inductive type"

  match constructorInfos with
  | [ctor] =>
    -- Single constructor case
    mkSingleConstructorInstance ctor typeId
  | _ =>
    -- Multiple constructors case - build it exactly like the single case but with multiple patterns
    let mut encodePatterns := #[]
    let mut decodeStatements := #[]
    let mut ctorApps := #[]

    for ctor in constructorInfos do
      let ctorId := mkIdent ctor.name
      let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
      let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

      let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)
      ctorApps := ctorApps.push ctorApp

      -- Generate decode statements exactly like single case
      let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
        `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
      decodeStatements := decodeStatements.push (ctor.name.toString, ctor.numFields, decodeStmts.toArray)

      -- Generate encode pattern exactly like single case
      let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
      let encodePattern ← if ctor.numFields = 0 then
        pure ⟨ctorId⟩
      else
        let fieldArray := fieldTerms.toArray
        `($(ctorId) $fieldArray*)
      encodePatterns := encodePatterns.push (encodePattern, encodeElems.toArray, ctor.name.toString)

    -- Now build the instance command
    let encodeArms ← encodePatterns.mapM fun (pattern, elems, name) => do
      `(LeanSerial.SerialValue.compound $(quote name) #[$elems,*])

    let decodeArms ← decodeStatements.mapIdxM fun i (name, numFields, stmts) => do
      let app := ctorApps[i]!
      `(doSeq|
        if args.size = $(quote numFields) then do
          $[$stmts:doElem]*
          .ok $app
        else
          .error "Field count mismatch")

    -- Build instance command
    let cmd ← `(
      instance : LeanSerial.Serializable $typeId where
        encode v := match v with
          $[| $(encodePatterns.map (·.1)) => $(encodeArms)]*
        decode sv := do
          let .compound ctor args := sv | .error "Expected compound value"
          match ctor with
          $[| $(decodeStatements.map (fun (name, _, _) => quote name)) => $(decodeArms)]*
          | _ => .error "Unknown constructor"
    )
    elabCommand cmd


def mkSerializableInstanceHandler (declName : Name) : CommandElabM Bool := do
  let env ← getEnv
  if isStructure env declName then
    let ctorVal := getStructureCtor env declName
    mkSerializableInstance ctorVal declName
    return true
  else
    match env.find? declName with
    | some (ConstantInfo.inductInfo _) =>
      mkSerializableInstanceForInductive declName
      -- throwError s!"Inductive types not supported yet: {declName}"
      return true
    | _ =>
      -- Fallback for non-structure types that might have a 'mk' constructor
      match env.find? (declName ++ `mk) with
      | some (ConstantInfo.ctorInfo ctorVal) =>
        mkSerializableInstance ctorVal declName
        return true
      | _ => return false


initialize
  registerDerivingHandler ``LeanSerial.Serializable fun declNames => do
    for declName in declNames do
      unless ← mkSerializableInstanceHandler declName do
        throwError "Failed to generate Serializable instance for {declName}"
    return true
