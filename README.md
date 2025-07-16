<h1 align="center">
  LeanSerial
</h1>

<h3 align="center">Type-safe serialization for Lean</h3>

<p align="center">
  <a href="https://github.com/leanprover/lean4/releases/tag/v4.21.0">
    <img src="https://img.shields.io/badge/Lean-v4.21.0-blue" alt="license" />
  </a>
  <a href="https://github.com/oOo0oOo/LeanSerial/blob/master/LICENSE">
    <img src="https://img.shields.io/github/license/oOo0oOo/LeanSerial.svg" alt="license" />
  </a>
</p>

## WIP! Expect breaking changes and bugs.

Feedback welcome!

## Key Features

* **Type Safety**: Compile-time guarantees with automatic deriving for custom types
* **Multiple Formats**: Serialize to ByteArray (CBOR), JSON, or String

## Usage

Add `LeanSerial` as a dependency to your `lakefile.toml`:

```toml
[[require]]
name = "LeanSerial"
git = "https://github.com/oOo0oOo/LeanSerial.git"
rev = "main"
```

Use it as follows:

```lean
import LeanSerial

-- Custom types by deriving `LeanSerial.Serializable`
structure FileNode where
  name : String
  children : Array FileNode  -- Recursive!
  validated : Option Bool
  deriving LeanSerial.Serializable, BEq

def main : IO Unit := do
  -- Create a recursive file structure
  let file1 : FileNode := { name := "file1.txt", children := #[], validated := some false }
  let file2 : FileNode := { name := "file2.bin", children := #[file1], validated := true }

  -- Serialize to different formats
  let bytes : ByteArray := LeanSerial.serialize file2 -- Binary format (CBOR)
  let json : Lean.Json := LeanSerial.serialize file2
  let string : String := LeanSerial.serialize file2

  match LeanSerial.deserialize bytes with
  | .ok (node: FileNode) =>
    if node == file2 then
      IO.println "Roundtrip successful!"
    else
      IO.println "Roundtrip failed!"
  | .error msg => IO.println s!"Error: {msg}"

  -- Serialize directly to/from file
  LeanSerial.serializeToFile file2 "serialized.cbor"
  LeanSerial.serializeToJsonFile file2 "serialized.json"

  match (← LeanSerial.deserializeFromFile "serialized.cbor") with
  | .ok node =>
    if node == file2 then
      IO.println "File roundtrip successful!"
  | .error msg => IO.println s!"Error loading file: {msg}"

  -- Supports variety of types
  let _ : ByteArray := LeanSerial.serialize [3, 1, 4]
  let _ : ByteArray := LeanSerial.serialize #[1.1, 2.2, 3.2]
  let _ : ByteArray := LeanSerial.serialize (Sum.inl 42 : Sum Nat String)
  let _ : ByteArray := LeanSerial.serialize (.ok "success" : Except String String)
  let _ : ByteArray := LeanSerial.serialize (("key", 123), ("value", 456))
  let _ : ByteArray := LeanSerial.serialize [true, false, true]
  let _ : ByteArray := LeanSerial.serialize [[1, 2], [3, 4], []]
  let _ : ByteArray := LeanSerial.serialize (System.FilePath.mk "/tmp/test.txt")
  let _ : ByteArray := LeanSerial.serialize (Std.Time.PlainDateTime.ofDaysSinceUNIXEpoch 1000 ⟨0, 0, 0, 0⟩)
  let _ : ByteArray := LeanSerial.serialize (some (some (some 42)))
```

## Supported Types

| Type Name                                 | Status      |
|-------------------------------------------|-------------|
| Nat, UInt8, UInt16, UInt32, UInt64        | ok          |
| Int, Int8, Int16, Int32, Int64            | ok          |
| Float                                     | ok          |
| String, Char, Bool, Unit                  | ok          |
| FilePath, Substring, String.Pos           | ok          |
| Option, Except                            | ok          |
| List, Array, Subarray, ByteArray, Fin     | ok          |
| Product (α × β), Sum (α ⊕ β), Thunk       | ok          |
| HashMap, HashSet, RBMap, RBTree           | ok          |
| PersistentHashMap, PersistentArray        | ok          |
| Std.Format.FlattenBehavior                | ok          |
| Lean.Json                                 | ok          |
| Lean.Position                             | ok          |
| Std.Time.Year.Era, Year.Offset            | ok          |
| Std.Time.Month.Ordinal, Week.Ordinal      | ok          |
| Std.Time.Weekday, Day.Ordinal             | ok          |
| Std.Time.Hour.Ordinal, Minute.Ordinal     | ok          |
| Std.Time.Second.Ordinal, Millisecond.Ordinal, Nanosecond.Ordinal | ok |
| Std.Time.Timestamp                        | ok          |
| Std.Time.PlainDate, PlainTime, PlainDateTime | ok       |
| Std.Time.TimeZone, TimeZone.ZoneRules     | ok          |
| Std.Time.ZonedDateTime                    | ok          |
| Lean.Name, LevelMVarId, Level             | ok          |
| Lean.Syntax.Preresolved, Syntax           | ok          |
| Lean.FVarId, MVarId, BinderInfo, Literal, DataValue | ok |
| Lean.KVMap, Options                       | ok          |
| Lean.Expr                                 | ok          |
| Lean.LocalDeclKind, LocalDecl, LocalContext, MetavarKind, LocalInstance, MetavarDecl, DelayedMetavarAssignment, MetavarContext | ok |
| Lean.ConstantVal, AxiomVal, ReducibilityHints, DefinitionSafety, TheoremVal, OpaqueVal, QuotKind, QuotVal, ConstructorVal, InductiveVal, DefinitionVal, RecursorRule, RecursorVal, ConstantInfo | ok |
| Lean.Import, ModuleIdx, CompactedRegion | ok |
| Lean.ModuleData, EnvironmentHeader, EnvironmentData, Environment | partial |
| Lean.NameSet, NameMap String, SMap        | ok          |
| Kernel.Diagnostics                        | ok          |
| StateM σ α, Dynamic                       | placeholder |
| DeclarationRange, DeclarationLocation, MacroExpansionInfo, FieldInfo, OptionInfo, FieldRedeclInfo, FVarAliasInfo, ElabInfo, ChoiceInfo, PartialTermInfo, TermInfo, CompletionInfo, DelabTermInfo, TacticInfo, CommandInfo, NameGenerator, OpenDecl, FileMap | ok |
| Lean.CommandContextInfo, PartialContextInfo | partial     |
| Lean.Widget.WidgetInstance, Elab.UserWidgetInfo, Elab.CustomInfo, Lean.Elab.Info | partial |
| Lean.Elab.InfoTree                        | partial       |
| TSyntax ks, TSyntaxArray ks               | ok          |
| Lean.FVarIdMap α, MVarIdMap α, FVarIdSet, MVarIdSet | ok |

**Legend:**
- `ok`: Type-safe and serialization/deserialization feasible.
- `partial`: While type-safe, some field values or children are not serialized/deserialized.
- `placeholder`: Only type info or dummy value is encoded, not real data.

## Custom Types without deriving

If deriving fails, you can manually implement `Serializable` for your types:

```lean
namespace LeanSerial

instance [Serializable α] : Serializable (MyArray α) where
  encode xs := .compound "MyArray" (xs.map encode)
  decode sv := do
    let args ← decodeCompound "MyArray" sv
    args.mapM decode |>.mapError (·)
```

## License

MIT