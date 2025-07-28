import Lean
import Lean.Elab.Command
import Lean.Elab.Deriving.Basic
import LeanSerde.Core

open Lean Elab Meta Term Command

structure ConstructorData where
  encodePattern : TSyntax `term
  encodeElems : Array (TSyntax `term)
  name : String
  ctorApp : TSyntax `term
  decodeStmts : Array (TSyntax `doElem)

private def mkFieldName (i : Nat) : Name := Name.mkSimple s!"x{i}"

private def mkAuxFunctionName (name: String) (typeName : Name) : Ident :=
  let typeNameStr := typeName.toString.replace "." "_"
  mkIdent (Name.mkSimple s!"{name}_{typeNameStr}")

private def generateContainerEncode (fieldType : Expr) (fieldTerm : TSyntax `term) (encodeFnName : Ident) : CommandElabM (TSyntax `term) := do
  match fieldType with
  | .app (.const `List _) _ =>
    `(LeanSerde.SerialValue.compound "List" (($fieldTerm).map $encodeFnName:ident |>.toArray))
  | .app (.const `Array _) _ =>
    `(LeanSerde.SerialValue.compound "Array" (($fieldTerm).map $encodeFnName:ident))
  | .app (.const `Option _) _ =>
    `(Option.casesOn $fieldTerm
        (LeanSerde.SerialValue.compound "Option.none" #[])
        (fun x => LeanSerde.SerialValue.compound "Option.some" #[$encodeFnName:ident x]))
  | .app (.app (.const `Prod _) _) _ =>
    `(LeanSerde.SerialValue.compound "Prod" #[$encodeFnName:ident ($fieldTerm).fst, $encodeFnName:ident ($fieldTerm).snd])
  | .app (.app (.const `Sum _) _) _ =>
    `(Sum.casesOn $fieldTerm
        (fun x => LeanSerde.SerialValue.compound "Sum.inl" #[$encodeFnName:ident x])
        (fun x => LeanSerde.SerialValue.compound "Sum.inr" #[$encodeFnName:ident x]))
  | _ => throwError "Invalid container type"

private def generateContainerDecode (fieldType : Expr) (fieldId : Ident) (index : Nat) (decodeFnName : Ident) : CommandElabM (Array (TSyntax `doElem)) := do
  match fieldType with
  | .app (.const `List _) _ => pure #[
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let containerArgs ← LeanSerde.decodeCompound "List" containerSv),
      ← `(doElem| let listResult ← containerArgs.mapM $decodeFnName:ident),
      ← `(doElem| let $fieldId := listResult.toList)
    ]
  | .app (.const `Array _) _ => pure #[
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let containerArgs ← LeanSerde.decodeCompound "Array" containerSv),
      ← `(doElem| let $fieldId ← containerArgs.mapM $decodeFnName:ident)
    ]
  | .app (.const `Option _) _ => pure #[
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let $fieldId ← match containerSv with
        | LeanSerde.SerialValue.compound "Option.none" args =>
          if args.size == 0 then return .none else throw "Invalid Option.none format"
        | LeanSerde.SerialValue.compound "Option.some" args =>
          if args.size == 1 then do
            let val ← $decodeFnName:ident args[0]!
            return (.some val)
          else throw "Invalid Option.some format"
        | _ => throw "Expected Option constructor")
    ]
  | .app (.app (.const `Prod _) _) _ => pure #[
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let containerArgs ← LeanSerde.decodeCompound "Prod" containerSv),
      ← `(doElem| let $fieldId ← do
        if containerArgs.size == 2 then do
          let fst ← $decodeFnName:ident containerArgs[0]!
          let snd ← $decodeFnName:ident containerArgs[1]!
          return (fst, snd)
        else
          throw "Invalid Prod format")
    ]
  | .app (.app (.const `Sum _) _) _ => pure #[
      ← `(doElem| let containerSv := args[$(quote index)]!),
      ← `(doElem| let $fieldId ← match containerSv with
        | LeanSerde.SerialValue.compound "Sum.inl" args =>
          if args.size == 1 then do
            let val ← $decodeFnName:ident args[0]!
            return (.inl val)
          else throw "Invalid Sum.inl format"
        | LeanSerde.SerialValue.compound "Sum.inr" args =>
          if args.size == 1 then do
            let val ← $decodeFnName:ident args[0]!
            return (.inr val)
          else throw "Invalid Sum.inr format"
        | _ => throw "Expected Sum constructor")
    ]
  | _ => throwError "Invalid container type"

private def extractTypeParameters (inductVal : InductiveVal) : CommandElabM (Array Name) := do
  if inductVal.numParams = 0 then
    return #[]

  let firstCtorName := inductVal.ctors[0]!
  let ctorInfo ← getConstInfoCtor firstCtorName

  liftTermElabM do
    forallTelescopeReducing ctorInfo.type fun xs _ => do
      let mut typeParams : Array Name := #[]
      for i in [:inductVal.numParams] do
        let x := xs[i]!
        let localDecl ← x.fvarId!.getDecl
        typeParams := typeParams.push localDecl.userName
      return typeParams

private def mkConstructorData (inductVal : InductiveVal) (ctor : ConstructorVal) (isStructure : Bool) : CommandElabM ConstructorData := do
  let ctorId := mkIdent ctor.name

  if ctor.numFields = 0 then
    return {
      encodePattern := ⟨ctorId⟩,
      encodeElems := #[],
      name := ctor.name.toString.split (· == '.') |>.getLast!,
      ctorApp := ⟨ctorId⟩,
      decodeStmts := #[]
    }

  let fieldIds := (Array.range ctor.numFields).map (mkIdent ∘ mkFieldName)
  let fieldTerms := fieldIds.map fun fieldId => ⟨fieldId⟩

  let encodeFnName := mkAuxFunctionName "encode" inductVal.name
  let decodeFnName := mkAuxFunctionName "decode" inductVal.name

  let ctorInfo ← getConstInfoCtor ctor.name
  let fieldTypes ← liftTermElabM do
    forallTelescopeReducing ctorInfo.type fun xs _ => do
      let mut types : Array Expr := #[]
      for i in [:ctor.numFields] do
        let x := xs[inductVal.numParams + i]!
        let localDecl ← x.fvarId!.getDecl
        types := types.push localDecl.type
      return types

  let result ← fieldTerms.zip fieldIds |>.zip fieldTypes |>.mapIdxM fun i ((fieldTerm, fieldId), fieldType) => do
    let isDirectRecursive := fieldType.isAppOf inductVal.name
    let isSimpleContainer := match fieldType with
      | .app (.const name _) inner =>
        (name == `List || name == `Array || name == `Option) && inner.isAppOf inductVal.name
      | .app (.app (.const name _) _) inner =>
        (name == `Prod || name == `Sum) && inner.isAppOf inductVal.name
      | _ => false

    let encodeElem ← if isDirectRecursive then
      `($encodeFnName $fieldTerm)
    else if isSimpleContainer then
      generateContainerEncode fieldType fieldTerm encodeFnName
    else
      `(LeanSerde.encode $fieldTerm)

    let decodeStmt ← if isDirectRecursive then
      pure #[← `(doElem| let $fieldId ← $decodeFnName:ident (args[$(quote i)]!))]
    else if isSimpleContainer then
      let stmts ← generateContainerDecode fieldType fieldId i decodeFnName
      pure stmts
    else
      pure #[← `(doElem| let $fieldId ← LeanSerde.decode (args[$(quote i)]!))]

    return (encodeElem, decodeStmt)

  let (encodeElems, decodeStmts) := result.unzip
  let decodeStmts := decodeStmts.flatten

  let ctorApp ← if ctor.numFields == 0 then
    pure (⟨ctorId⟩ : TSyntax `term)
  else
    `($ctorId $fieldTerms*)

  let encodePattern ←
    if ctor.numFields = 0 then
      pure ⟨ctorId⟩
    else
      `($ctorId $fieldTerms*)

  let ctorName := if isStructure then
    inductVal.name.toString
  else
    ctor.name.toString

  return {
    encodePattern := encodePattern,
    encodeElems := encodeElems,
    name := ctorName,
    ctorApp := ctorApp,
    decodeStmts := decodeStmts
  }

private def mkSerializableQuotation (typeId : TSyntax `ident) (constructorData : Array ConstructorData) (constructorInfos : Array ConstructorVal) (isRecursive : Bool) (typeParams : Array Name) (typeName : Name) : CommandElabM (Array (TSyntax `command)) := do
  let encodeMatches := constructorData.map fun cd =>
    (cd.encodePattern, cd.encodeElems, cd.name)

  let encodeArms ← encodeMatches.mapM fun (_, elems, name) =>
    `(LeanSerde.SerialValue.compound $(quote name) #[$elems,*])

  let decodeArms ← constructorData.mapIdxM fun i data => do
    let numFields := constructorInfos[i]!.numFields
    if data.decodeStmts.isEmpty then
      `(if args.size != $(quote numFields) then throw "Field count mismatch" else return $(data.ctorApp))
    else
      `(if args.size != $(quote numFields) then throw "Field count mismatch" else do
          $[$(data.decodeStmts):doElem]*
          return $(data.ctorApp))

  let decodePatterns ← constructorData.mapM fun data => `($(quote data.name))
  let encodePatterns := constructorData.map (·.encodePattern)

  let encodeFnName := mkAuxFunctionName "encode" typeName
  let decodeFnName := mkAuxFunctionName "decode" typeName

  let polyTypeApp ← if typeParams.isEmpty then
    pure ⟨typeId⟩
  else
    let paramIds := typeParams.map mkIdent
    `($typeId $paramIds*)
  let instConstraints ← typeParams.mapM fun param => do
    let paramId := mkIdent param
    `(bracketedBinder| [LeanSerde.Serializable $paramId])

  let encodeDef ← if isRecursive then
    if typeParams.isEmpty then
      `(private partial def $encodeFnName (v : $typeId) : LeanSerde.SerialValue :=
          match v with
          $[| $encodePatterns => $encodeArms]*)
    else
      `(private partial def $encodeFnName $instConstraints:bracketedBinder* (v : $polyTypeApp) : LeanSerde.SerialValue :=
          match v with
          $[| $encodePatterns => $encodeArms]*)
  else
    if typeParams.isEmpty then
      `(private def $encodeFnName (v : $typeId) : LeanSerde.SerialValue :=
          match v with
          $[| $encodePatterns => $encodeArms]*)
    else
      `(private def $encodeFnName $instConstraints:bracketedBinder* (v : $polyTypeApp) : LeanSerde.SerialValue :=
          match v with
          $[| $encodePatterns => $encodeArms]*)

  let decodeMatcher ← if constructorInfos.size == 1 then
    pure decodeArms[0]!
  else
    `(match ctor with
      $[| $decodePatterns => $decodeArms]*
      | _ => .error "Unknown constructor")

  let decodeDef ← if isRecursive then
    if typeParams.isEmpty then
      `(private partial def $decodeFnName (sv : LeanSerde.SerialValue) : Except String $typeId :=
          match sv with
          | .compound ctor args => $decodeMatcher
          | _ => .error "Expected compound value")
    else
      `(private partial def $decodeFnName $instConstraints:bracketedBinder* (sv : LeanSerde.SerialValue) : Except String $polyTypeApp :=
          match sv with
          | .compound ctor args => $decodeMatcher
          | _ => .error "Expected compound value")
  else
    if typeParams.isEmpty then
      `(private def $decodeFnName (sv : LeanSerde.SerialValue) : Except String $typeId :=
          match sv with
          | .compound ctor args => $decodeMatcher
          | _ => .error "Expected compound value")
    else
      `(private def $decodeFnName $instConstraints:bracketedBinder* (sv : LeanSerde.SerialValue) : Except String $polyTypeApp :=
          match sv with
          | .compound ctor args => $decodeMatcher
          | _ => .error "Expected compound value")

  let inst ← if typeParams.isEmpty then
    `(instance : LeanSerde.Serializable $typeId where
        encode := $encodeFnName
        decode := $decodeFnName)
  else
    `(instance $instConstraints:bracketedBinder* : LeanSerde.Serializable $polyTypeApp where
        encode := $encodeFnName
        decode := $decodeFnName)

  return #[encodeDef, decodeDef, inst]

def mkSerializableInstance (typeName : Name) : CommandElabM Unit := do
  let env ← getEnv

  let some constInfo := env.find? typeName
    | throwError s!"Type {typeName} not found in environment. Make sure the type is properly imported and defined."

  match constInfo with
  | ConstantInfo.inductInfo inductVal =>
    let typeId := mkIdent typeName
    let typeParams ← extractTypeParameters inductVal

    let constructorInfosArray ← inductVal.ctors.toArray.mapM fun ctorName => do
      let some (ConstantInfo.ctorInfo ctorVal) := env.find? ctorName
        | throwError s!"Constructor {ctorName} not found for inductive type {typeName}. This is likely an internal error."
      return ctorVal

    if constructorInfosArray.isEmpty then
      throwError s!"Inductive type {typeName} has no constructors. Empty inductive types cannot be serialized."

    let isStructure := isStructure env typeName
    let constructorData ← constructorInfosArray.mapM (mkConstructorData inductVal · isStructure)
    let cmds ← mkSerializableQuotation typeId constructorData constructorInfosArray inductVal.isRec typeParams typeName

    cmds.forM elabCommand
  | _ =>
    throwError s!"Type {typeName} is not an inductive type. Serializable instances can only be created for inductive types."

initialize
  registerDerivingHandler ``LeanSerde.Serializable fun declNames => do
    for declName in declNames do
      mkSerializableInstance declName
    return true
