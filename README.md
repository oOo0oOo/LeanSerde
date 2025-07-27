<h1 align="center">
  LeanSerde
</h1>

<h3 align="center">Type-safe serialization for Lean</h3>

<p align="center">
  <a href="https://github.com/leanprover/lean4/releases/tag/v4.21.0">
    <img src="https://img.shields.io/badge/Lean-v4.21.0-blue" alt="Lean version" />
  </a>
  <a href="">
    <img src="https://img.shields.io/github/last-commit/oOo0oOo/LeanSerde" alt="last update" />
  </a>
  <a href="https://github.com/oOo0oOo/LeanSerde/blob/main/LICENSE">
    <img src="https://img.shields.io/github/license/oOo0oOo/LeanSerde.svg" alt="license" />
  </a>
</p>

## WIP!

Feedback welcome!

## Key Features

* **Type Safety**: Compile-time guarantees with automatic deriving for custom types
* **Deduplication**: Efficient serialization of shared structures
* **Multiple Formats**: Serialize to ByteArray (CBOR), Lean.Json, or String

## Usage

Add `LeanSerde` as a dependency to your `lakefile.toml`:

```toml
[[require]]
name = "LeanSerde"
git = "https://github.com/oOo0oOo/LeanSerde.git"
rev = "main"
```

Use it as follows:

```lean
import LeanSerde

-- Custom types by deriving `LeanSerde.Serializable`
structure FileNode where
  name : String
  children : Array FileNode  -- Recursive!
  validated : Option Bool
  deriving LeanSerde.Serializable, BEq

def main : IO Unit := do
  -- Create a recursive file structure
  let file1 : FileNode := { name := "file1.txt", children := #[], validated := some false }
  let file2 : FileNode := { name := "file2.bin", children := #[file1], validated := true }

  -- Serialize to different formats
  let bytes : ByteArray := LeanSerde.serialize file2 -- Binary format (CBOR)
  let json : Lean.Json := LeanSerde.serialize file2
  let string : String := LeanSerde.serialize file2

  match LeanSerde.deserialize bytes with
  | .ok (node: FileNode) =>
    if node == file2 then
      IO.println "Roundtrip successful!"
    else
      IO.println "Roundtrip failed!"
  | .error msg => IO.println s!"Error: {msg}"

  -- Serialize directly to/from file
  LeanSerde.serializeToFile file2 "serialized.cbor"
  LeanSerde.serializeToJsonFile file2 "serialized.json"

  match (← LeanSerde.deserializeFromFile "serialized.cbor") with
  | .ok node =>
    if node == file2 then
      IO.println "File roundtrip successful!"
  | .error msg => IO.println s!"Error loading file: {msg}"

  -- Supports variety of types
  let _ : ByteArray := LeanSerde.serialize [3, 1, 4]
  let _ : ByteArray := LeanSerde.serialize #[1.1, 2.2, 3.2]
  let _ : ByteArray := LeanSerde.serialize (Sum.inl 42 : Sum Nat String)
  let _ : ByteArray := LeanSerde.serialize (.ok "success" : Except String String)
  let _ : ByteArray := LeanSerde.serialize (("key", 123), ("value", 456))
  let _ : ByteArray := LeanSerde.serialize [true, false, true]
  let _ : ByteArray := LeanSerde.serialize [[1, 2], [3, 4], []]
  let _ : ByteArray := LeanSerde.serialize (System.FilePath.mk "/tmp/test.txt")
  let _ : ByteArray := LeanSerde.serialize (Std.Time.PlainDateTime.ofDaysSinceUNIXEpoch 1000 ⟨0, 0, 0, 0⟩)
  let _ : ByteArray := LeanSerde.serialize (some (some (some 42)))
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

### Meta Types

Use `import LeanSerde.MetaTypes` to access meta types:

| Type Name                                 | Status      |
|-------------------------------------------|-------------|
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
namespace LeanSerde

instance [Serializable α] : Serializable (MyArray α) where
  encode xs := .compound "MyArray" (xs.map encode)
  decode sv := do
    let args ← decodeCompound "MyArray" sv
    let decoded ← args.mapM decode
    return decoded
```

## Serialization Format

LeanSerde automatically chooses between direct serialization or deduplication based on repeated values.

### SerialValue Types

- **`str`**: String values
- **`nat`**: Natural numbers
- **`bool`**: Boolean values
- **`compound`**: Named constructors with child values (e.g., `["FileNode", [name, children, validated]]`)
- **`ref`**: References to shared objects by consecutive ID starting from 0 (e.g., `{"ref": 0}`)

### Formats

**Simple Format**: Direct serialization without repeating values.

**Graph Format**: Deduplicating format when values are shared. Creates a `GraphData` structure with:
- **`root`**: The main `SerialValue` with refs to shared objects
- **`objects`**: Array of shared `SerialValue`s

This intermediate representation then gets encoded to the final output format (JSON, CBOR, String).

Deserialization automatically handles all output formats.

### Examples

```lean
let file1 : FileNode := { name := "file1.txt", children := #[], validated := some false }
let file2 : FileNode := { name := "file2.bin", children := #[file1], validated := some true }
let file3 : FileNode := { name := "file3.txt", children := #[file1, file1], validated := some false }
```

**Simple format** (no sharing):
```jsonc
// file1
["FileNode", ["file1.txt", ["Array", []], ["Option.some", [false]]]]

// file2
["FileNode",
 ["file2.bin",
  ["Array",
   [["FileNode", ["file1.txt", ["Array", []], ["Option.some", [false]]]]]],
  ["Option.some", [true]]]]
```

**Graph format** (file1 appears twice in file3):
```jsonc
// file3
{"root":
 ["FileNode",
  ["file3.txt", ["Array", [{"ref": 0}, {"ref": 0}]], ["Option.some", [true]]]],
 "objects":
 [["FileNode", ["file1.txt", ["Array", []], ["Option.some", [false]]]]]}
```

## Testing

Clone the repository and run:

```bash
lake test
```

## License

MIT