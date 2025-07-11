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
          return $ctorApp
        else
          .error "Field count mismatch"
  )
  elabCommand cmd

-- Single constructor case
def mkSerializableInstanceSingle (typeId : Ident) (ctor : ConstructorVal) : CommandElabM Unit := do
  let ctorId := mkIdent ctor.name
  let fieldCount := ctor.numFields

  let fieldIds := (List.range fieldCount).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

  let encodeElems ← fieldTerms.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts ← fieldIds.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)

  let ctorApp ← fieldTerms.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctorId⟩

  let pattern ←
    match fieldCount with
    | 0 => pure ⟨ctorId⟩
    | _ => `($ctorId $fieldTerms.toArray*)

  let cmd ← `(
    instance : LeanSerial.Serializable $typeId where
      encode v := match v with
        | $pattern => LeanSerial.SerialValue.compound $(quote ctor.name.toString) #[$encodeElems.toArray,*]
      decode sv := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        | $(quote ctor.name.toString) =>
          if args.size = $(quote fieldCount) then do
            $[$(decodeStmts.toArray):doElem]*
            return $ctorApp
          else
            .error "Field count mismatch"
        | _ => .error "Unknown constructor"
  )
  elabCommand cmd

-- Two constructors case
def mkSerializableInstanceTwo (typeId : Ident) (ctor0 ctor1 : ConstructorVal) : CommandElabM Unit := do
  let ctor0Id := mkIdent ctor0.name
  let ctor1Id := mkIdent ctor1.name

  -- Handle ctor0
  let fieldIds0 := (List.range ctor0.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms0 := fieldIds0.map fun fieldId => ⟨fieldId⟩
  let encodeElems0 ← fieldTerms0.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts0 ← fieldIds0.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
  let ctorApp0 ← fieldTerms0.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctor0Id⟩

  let pattern0 ←
    match ctor0.numFields with
    | 0 => pure ⟨ctor0Id⟩
    | _ => `($ctor0Id $fieldTerms0.toArray*)

  -- Handle ctor1
  let fieldIds1 := (List.range ctor1.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms1 := fieldIds1.map fun fieldId => ⟨fieldId⟩
  let encodeElems1 ← fieldTerms1.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts1 ← fieldIds1.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
  let ctorApp1 ← fieldTerms1.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctor1Id⟩

  let pattern1 ←
    match ctor1.numFields with
    | 0 => pure ⟨ctor1Id⟩
    | _ => `($ctor1Id $fieldTerms1.toArray*)

  let cmd ← `(
    instance : LeanSerial.Serializable $typeId where
      encode v := match v with
        | $pattern0 => LeanSerial.SerialValue.compound $(quote ctor0.name.toString) #[$encodeElems0.toArray,*]
        | $pattern1 => LeanSerial.SerialValue.compound $(quote ctor1.name.toString) #[$encodeElems1.toArray,*]
      decode sv := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        | $(quote ctor0.name.toString) =>
          if args.size = $(quote ctor0.numFields) then do
            $[$(decodeStmts0.toArray):doElem]*
            return $ctorApp0
          else
            .error "Field count mismatch"
        | $(quote ctor1.name.toString) =>
          if args.size = $(quote ctor1.numFields) then do
            $[$(decodeStmts1.toArray):doElem]*
            return $ctorApp1
          else
            .error "Field count mismatch"
        | _ => .error "Unknown constructor"
  )
  elabCommand cmd

-- Three constructors case
def mkSerializableInstanceThree (typeId : Ident) (ctor0 ctor1 ctor2 : ConstructorVal) : CommandElabM Unit := do
  let ctor0Id := mkIdent ctor0.name
  let ctor1Id := mkIdent ctor1.name
  let ctor2Id := mkIdent ctor2.name

  -- Handle ctor0
  let fieldIds0 := (List.range ctor0.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms0 := fieldIds0.map fun fieldId => ⟨fieldId⟩
  let encodeElems0 ← fieldTerms0.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts0 ← fieldIds0.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
  let ctorApp0 ← fieldTerms0.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctor0Id⟩
  let pattern0 ← match ctor0.numFields with | 0 => pure ⟨ctor0Id⟩ | _ => `($ctor0Id $fieldTerms0.toArray*)

  -- Handle ctor1
  let fieldIds1 := (List.range ctor1.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms1 := fieldIds1.map fun fieldId => ⟨fieldId⟩
  let encodeElems1 ← fieldTerms1.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts1 ← fieldIds1.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
  let ctorApp1 ← fieldTerms1.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctor1Id⟩
  let pattern1 ← match ctor1.numFields with | 0 => pure ⟨ctor1Id⟩ | _ => `($ctor1Id $fieldTerms1.toArray*)

  -- Handle ctor2
  let fieldIds2 := (List.range ctor2.numFields).map fun i => mkIdent (Name.mkSimple s!"field{i}")
  let fieldTerms2 := fieldIds2.map fun fieldId => ⟨fieldId⟩
  let encodeElems2 ← fieldTerms2.mapM fun fieldTerm => `(LeanSerial.encode $fieldTerm)
  let decodeStmts2 ← fieldIds2.mapIdxM fun i fieldId => do
    `(doElem| let $fieldId ← LeanSerial.decode args[$(quote i)]!)
  let ctorApp2 ← fieldTerms2.foldlM (fun acc fieldTerm => `($acc $fieldTerm)) ⟨ctor2Id⟩
  let pattern2 ← match ctor2.numFields with | 0 => pure ⟨ctor2Id⟩ | _ => `($ctor2Id $fieldTerms2.toArray*)

  let cmd ← `(
    instance : LeanSerial.Serializable $typeId where
      encode v := match v with
        | $pattern0 => LeanSerial.SerialValue.compound $(quote ctor0.name.toString) #[$encodeElems0.toArray,*]
        | $pattern1 => LeanSerial.SerialValue.compound $(quote ctor1.name.toString) #[$encodeElems1.toArray,*]
        | $pattern2 => LeanSerial.SerialValue.compound $(quote ctor2.name.toString) #[$encodeElems2.toArray,*]
      decode sv := do
        let .compound ctor args := sv | .error "Expected compound value"
        match ctor with
        | $(quote ctor0.name.toString) =>
          if args.size = $(quote ctor0.numFields) then do
            $[$(decodeStmts0.toArray):doElem]*
            return $ctorApp0
          else
            .error "Field count mismatch"
        | $(quote ctor1.name.toString) =>
          if args.size = $(quote ctor1.numFields) then do
            $[$(decodeStmts1.toArray):doElem]*
            return $ctorApp1
          else
            .error "Field count mismatch"
        | $(quote ctor2.name.toString) =>
          if args.size = $(quote ctor2.numFields) then do
            $[$(decodeStmts2.toArray):doElem]*
            return $ctorApp2
          else
            .error "Field count mismatch"
        | _ => .error "Unknown constructor"
  )
  elabCommand cmd

def mkSerializableInstanceForInductive (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv
  let some (ConstantInfo.inductInfo inductVal) := env.find? typeName | throwError "not an inductive type"

  let typeId := mkIdent typeName

  -- Handle all cases uniformly - no special cases
  let constructorInfos ← inductVal.ctors.mapM fun ctorName => do
    let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName | throwError s!"constructor {ctorName} not found"
    return ctorVal

  match constructorInfos with
  | [] => throwError "Empty inductive type"
  | [ctor] =>
    -- Single constructor - use direct approach
    mkSerializableInstanceSingle typeId ctor
  | [ctor0, ctor1] =>
    -- Two constructors - build directly
    mkSerializableInstanceTwo typeId ctor0 ctor1
  | [ctor0, ctor1, ctor2] =>
    -- Three constructors - build directly
    mkSerializableInstanceThree typeId ctor0 ctor1 ctor2
  | _ =>
    throwError s!"Inductive types with {constructorInfos.length} constructors not yet supported - add more cases as needed"

def mkSerializableInstanceHandler (declName : Name) : CommandElabM Bool := do
  let env ← getEnv
  match env.find? declName with
  | some (ConstantInfo.inductInfo _) =>
    mkSerializableInstanceForInductive declName
    return true
  | _ =>
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
