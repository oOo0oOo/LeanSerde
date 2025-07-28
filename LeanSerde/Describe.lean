import Lean
import Lean.Elab.Command
import Lean.Elab.Term
import LeanSerde.Core

namespace LeanSerde

open Lean Elab Command Term Meta

private partial def analyzeSerialValue (sv : SerialValue) (seen : Std.HashSet String := {}) : String :=
  match sv with
  | .str _ => "<String>"
  | .nat _ => "<Nat>"
  | .bool _ => "<Bool>"
  | .ref _ => "<Ref>"
  | .compound name children =>
    let shortName := name
    if seen.contains shortName then
      s!"<{shortName}>"
    else
      let newSeen := seen.insert shortName
      if children.isEmpty then
        s!"[\"{shortName}\", [<?>]]"
      else
        let childDescs := children.map (analyzeSerialValue · newSeen)
        let childStrs := String.intercalate ", " childDescs.toList
        s!"[\"{shortName}\", [{childStrs}]]"

def describeSerializableFormat (α : Type) [Serializable α] [Inhabited α] : String :=
  let dummyValue : α := default
  let serialValue := encode dummyValue
  analyzeSerialValue serialValue

elab "LeanSerde.describeFormat" "(" α:term ")" : term => do
  let typeExpr ← elabType α
  try
    let _serInst ← synthInstance (← mkAppM ``Serializable #[typeExpr])
    let inhabitedResult? ← try
      let _inhInst ← synthInstance (← mkAppM ``Inhabited #[typeExpr])
      pure (some ())
    catch _ =>
      pure none

    match inhabitedResult? with
    | some _ =>
      let stx ← `(describeSerializableFormat $(← exprToSyntax typeExpr))
      elabTerm stx none
    | none =>
      let typeName := typeExpr.getAppFn.constName?.getD `unknown
      let lastPart := typeName
      return mkStrLit s!"<{lastPart} (Serializable but not Inhabited)>"
  catch _ =>
    let typeName := typeExpr.getAppFn.constName?.getD `unknown
    let lastPart := typeName
    return mkStrLit s!"<{lastPart} (Not Serializable)>"

end LeanSerde
