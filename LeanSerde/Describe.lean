import Lean
import Lean.Data.Json
import Lean.Elab.Command
import Lean.Elab.Term
import LeanSerde.Core

namespace LeanSerde

open Lean Elab Command Term Meta

private partial def analyzeSerialValue (sv : SerialValue) (seen : Std.HashSet String := {}) : Lean.Json :=
  match sv with
  | .str _ => .str "<String>"
  | .nat _ => .str "<Nat>"
  | .bool _ => .str "<Bool>"
  | .ref _ => .str "<Ref>"
  | .compound name children =>
    if seen.contains name then
      .str s!"<{name}>"
    else
      let newSeen := seen.insert name
      if children.isEmpty then
        .arr #[.str name, .arr #[.str "<?>"]]
      else
        let childDescs := children.map (analyzeSerialValue · newSeen)
        .arr #[.str name, .arr childDescs]

private def describeSerializableFormat (α : Type) [Serializable α] [Inhabited α] : IO Lean.Json := do
  let dummyValue : α := default
  let serialValue ← encode dummyValue
  return analyzeSerialValue serialValue

elab "LeanSerde.describeFormat" α:term : term => do
  let typeExpr ← elabType α
  let stringExpr ← mkConst ``String
  let jsonExpr ← mkConst ``Lean.Json
  let exceptExpr ← mkAppM ``Except #[stringExpr, jsonExpr]
  let ioExceptType ← mkAppM ``IO #[exceptExpr]

  try
    let _serInst ← synthInstance (← mkAppM ``Serializable #[typeExpr])
    let inhabitedResult? ← try
      let _inhInst ← synthInstance (← mkAppM ``Inhabited #[typeExpr])
      pure (some ())
    catch _ =>
      pure none

    match inhabitedResult? with
    | some _ =>
      let typeStx ← exprToSyntax typeExpr
      let stx ← `(do
        let result ← describeSerializableFormat $(typeStx)
        return Except.ok result)
      elabTerm stx (some ioExceptType)
    | none =>
      let typeName := typeExpr.getAppFn.constName?.getD `unknown
      let errorMsg := s!"{typeName} is Serializable but missing Inhabited instance"
      let stx ← `(return Except.error $(quote errorMsg))
      elabTerm stx (some ioExceptType)
  catch _ =>
    let typeName := typeExpr.getAppFn.constName?.getD `unknown
    let errorMsg := s!"{typeName} is not Serializable"
    let stx ← `(return Except.error $(quote errorMsg))
    elabTerm stx (some ioExceptType)

end LeanSerde
