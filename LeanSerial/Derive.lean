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

-- def mkSerializableInstanceForInductive (typeName : Name) : CommandElabM Unit := do
--   let env ← getEnv
--   let some (ConstantInfo.inductInfo inductVal) := env.find? typeName | throwError "not an inductive type"

--   let typeId := mkIdent typeName

--   let constructorInfos ← inductVal.ctors.mapM fun ctorName => do
--     let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName | throwError s!"constructor {ctorName} not found"
--     return ctorVal

--   if constructorInfos.isEmpty then
--     throwError "Empty inductive type"

--   -- Build the instance manually with proper syntax
--   match constructorInfos with
--   | [ctor] =>
--     -- Single constructor case
--     let ctorId := mkIdent ctor.name
--     let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
--     let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

--     if ctor.numFields = 0 then
--       let cmd ← `(
--         instance : LeanSerial.Serializable $typeId where
--           encode
--             | $(ctorId) => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[]
--           decode sv := do
--             let .compound ctor args := sv | .error "Expected compound value"
--             match ctor with
--             | $(quote ctor.name.toString) => do
--               if args.size = 0 then .ok $(ctorId)
--               else .error "Field count mismatch"
--             | _ => .error "Unknown constructor"
--       )
--       elabCommand cmd
--     else
--       let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
--       let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
--         `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
--       let fieldArray := fieldTerms.toArray
--       let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

--       let cmd ← `(
--         instance : LeanSerial.Serializable $typeId where
--           encode
--             | $(ctorId) $fieldArray* => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]
--           decode sv := do
--             let .compound ctor args := sv | .error "Expected compound value"
--             match ctor with
--             | $(quote ctor.name.toString) => do
--               if args.size = $(quote ctor.numFields) then do
--                 $[$(decodeStmts.toArray):doElem]*
--                 .ok $ctorApp
--               else
--                 .error "Field count mismatch"
--             | _ => .error "Unknown constructor"
--       )
--       elabCommand cmd
--   | _ =>
--     -- Multiple constructors case - generate explicit instance
--     let mut encodeArms := #[]
--     let mut decodeArms := #[]

--     for ctor in constructorInfos do
--       let ctorId := mkIdent ctor.name
--       let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
--       let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

--       -- Generate encode arm
--       if ctor.numFields = 0 then
--         encodeArms := encodeArms.push (ctorId, none, ← `(LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[]))
--       else
--         let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
--         let fieldArray := fieldTerms.toArray
--         encodeArms := encodeArms.push (ctorId, some fieldArray, ← `(LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]))

--       -- Generate decode statements
--       let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
--         `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

--       let ctorApp ← if ctor.numFields = 0 then
--         pure ⟨ctorId⟩
--       else
--         fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

--       decodeArms := decodeArms.push (ctor.name.toString, ctor.numFields, decodeStmts.toArray, ctorApp)

--     -- Manually construct the command syntax using the single constructor working pattern
--     match constructorInfos with
--     | [] => throwError "Empty inductive type"
--     | [ctor] =>
--       -- Single constructor - use existing working code
--       let ctorId := mkIdent ctor.name
--       let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
--       let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

--       if ctor.numFields = 0 then
--         let cmd ← `(
--           instance : LeanSerial.Serializable $typeId where
--             encode
--               | $(ctorId) => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[]
--             decode sv := do
--               let .compound ctor args := sv | .error "Expected compound value"
--               match ctor with
--               | $(quote ctor.name.toString) => do
--                 if args.size = 0 then .ok $(ctorId)
--                 else .error "Field count mismatch"
--               | _ => .error "Unknown constructor"
--         )
--         elabCommand cmd
--       else
--         let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
--         let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
--           `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
--         let fieldArray := fieldTerms.toArray
--         let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

--         let cmd ← `(
--           instance : LeanSerial.Serializable $typeId where
--             encode
--               | $(ctorId) $fieldArray* => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]
--             decode sv := do
--               let .compound ctor args := sv | .error "Expected compound value"
--               match ctor with
--               | $(quote ctor.name.toString) => do
--                 if args.size = $(quote ctor.numFields) then do
--                   $[$(decodeStmts.toArray):doElem]*
--                   .ok $ctorApp
--                 else
--                   .error "Field count mismatch"
--               | _ => .error "Unknown constructor"
--         )
--         elabCommand cmd
--     | _ =>
--       -- Multiple constructors - implement general case
--       throwError s!"Multiple constructor types with {constructorInfos.length} constructors not yet fully supported"

-- -- Helper function for single constructor instance generation
-- def mkSingleConstructorInstance (ctor : ConstructorVal) (typeId : TSyntax `ident) : CommandElabM Unit := do
--   let ctorId := mkIdent ctor.name
--   let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
--   let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

--   if ctor.numFields = 0 then
--     let cmd ← `(
--       instance : LeanSerial.Serializable $typeId where
--         encode
--           | $(ctorId) => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[]
--         decode sv := do
--           let .compound ctor args := sv | .error "Expected compound value"
--           match ctor with
--           | $(quote ctor.name.toString) => do
--             if args.size = 0 then .ok $(ctorId)
--             else .error "Field count mismatch"
--           | _ => .error "Unknown constructor"
--     )
--     -- Log info
--     IO.println s!"Generating Serializable instance for {typeId} with single constructor {ctor.name}"
--     elabCommand cmd
--   else
--     let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
--     let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
--       `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
--     let fieldArray := fieldTerms.toArray
--     let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

--     let cmd ← `(
--       instance : LeanSerial.Serializable $typeId where
--         encode
--           | $(ctorId) $fieldArray* => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]
--         decode sv := do
--           let .compound ctor args := sv | .error "Expected compound value"
--           match ctor with
--           | $(quote ctor.name.toString) => do
--             if args.size = $(quote ctor.numFields) then do
--               $[$(decodeStmts.toArray):doElem]*
--               .ok $ctorApp
--             else
--               .error "Field count mismatch"
--           | _ => .error "Unknown constructor"
--     )
--     -- Log info
--     IO.println s!"Generating Serializable instance for {typeId} with multiple constructors {ctor.name}"
--     elabCommand cmd


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

  -- Log info
  let fieldDesc := if ctor.numFields = 0 then "no fields" else s!"{ctor.numFields} fields"
  IO.println s!"Generating Serializable instance for {typeId} with single constructor {ctor.name} ({fieldDesc})"
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
    -- Multiple constructors case
    let mut encodeArms := #[]
    let mut decodeArms := #[]

    for ctor in constructorInfos do
      let ctorId := mkIdent ctor.name
      let fieldIds := (List.range ctor.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
      let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

      -- Generate encode arm
      if ctor.numFields = 0 then
        encodeArms := encodeArms.push (ctorId, none, ← `(LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[]))
      else
        let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
        let fieldArray := fieldTerms.toArray
        encodeArms := encodeArms.push (ctorId, some fieldArray, ← `(LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$(encodeElems.toArray),*]))

      -- Generate decode statements
      let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
        `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

      let ctorApp ← if ctor.numFields = 0 then
        pure ⟨ctorId⟩
      else
        fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) (⟨ctorId⟩ : TSyntax `term)

      decodeArms := decodeArms.push (ctor.name.toString, ctor.numFields, decodeStmts.toArray, ctorApp)

    -- Multiple constructors - implement general case
    throwError s!"Multiple constructor types with {constructorInfos.length} constructors not yet fully supported"


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
