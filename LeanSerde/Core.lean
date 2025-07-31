import Lean.Data.Json
import Std.Data.HashMap

namespace LeanSerde

inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  | ref : Nat → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

partial def hashSerialValue : SerialValue → UInt64
  -- Structural hash using mixHash with variant-specific prefixes for better distribution
  | .str s => mixHash 1 (hash s)
  | .nat n => mixHash 2 (hash n)
  | .bool b => mixHash 3 (hash b)
  | .compound name children =>
    let childrenHash := children.foldl (fun acc child => mixHash acc (hashSerialValue child)) 7
    mixHash 4 (mixHash (hash name) childrenHash)
  | .ref id => mixHash 5 (hash id)

instance : Hashable SerialValue where
  hash := hashSerialValue

def toJson : SerialValue → Lean.Json
  | .str s => .str s
  | .nat n => .num n
  | .bool b => .bool b
  | .compound name children => .arr #[.str name, .arr (children.map toJson)]
  | .ref id => .mkObj [("ref", .num id)]

partial def fromJson : Lean.Json → Except String SerialValue
  | .str s => .ok (.str s)
  | .num n =>
    if n.exponent == 0 && n.mantissa >= 0 then .ok (.nat n.mantissa.natAbs)
    else .error s!"Expected natural number, got {n}"
  | .bool b => .ok (.bool b)
  | .arr #[.str name, .arr children] => do
    let args ← children.mapM fromJson
    .ok (.compound name args)
  | .arr arr => .error s!"Expected [name, args], got array of size {arr.size}"
  | .obj obj => do
    let some (Lean.Json.num id) := obj.find compare "ref"
      | .error "Invalid SerialValue JSON object"
    if id.exponent == 0 && id.mantissa >= 0 then .ok (.ref id.mantissa.natAbs)
    else .error s!"Expected natural number for ref, got {id}"
  | _ => .error "Invalid SerialValue JSON format"

instance : Lean.ToJson SerialValue := ⟨toJson⟩
instance : Lean.FromJson SerialValue := ⟨fromJson⟩

end SerialValue

structure GraphData where
  root : SerialValue
  objects : Array SerialValue
  deriving Lean.ToJson, Lean.FromJson

private structure EncodeState where
  seen : Std.HashMap SerialValue Nat
  nextId : Nat
  objects : Array SerialValue

private def EncodeState.empty : EncodeState := ⟨{}, 0, #[]⟩
private abbrev EncodeGraphM := StateM EncodeState
abbrev DecodeM := ExceptT String IO

class SerializableFormat (α : Type) where
  serializeValue : GraphData → α
  deserializeValue : α → Except String GraphData

private def serializeToJson (gd : GraphData) : Lean.Json :=
  if gd.objects.isEmpty then
    gd.root.toJson
  else
    Lean.toJson gd

private def deserializeFromJson (json : Lean.Json) : Except String GraphData :=
  match SerialValue.fromJson json with
  | .ok sv => .ok ⟨sv, #[]⟩
  | .error _ => Lean.fromJson? json

instance : SerializableFormat Lean.Json where
  serializeValue := serializeToJson
  deserializeValue := deserializeFromJson

instance : SerializableFormat String where
  serializeValue gd := (serializeToJson gd).pretty
  deserializeValue str := do
    let json ← Lean.Json.parse str
    deserializeFromJson json

-- For now all in IO :/
class Serializable (α : Type) where
  encode : α → IO SerialValue
  decode : SerialValue → ExceptT String IO α

export Serializable (encode decode)

private partial def countOccurrences (sv : SerialValue) (counts : Std.HashMap SerialValue Nat) : Std.HashMap SerialValue Nat :=
  -- Count occurrences of SerialValue in a structure
  match sv with
  | .str _ | .nat _ | .bool _ | .ref _ => counts
  | .compound _name children =>
    let counts := counts.insert sv (counts.getD sv 0 + 1)
    children.foldl (fun acc child => countOccurrences child acc) counts

private partial def encodeWithRefs (sv : SerialValue) (shared : Std.HashMap SerialValue Nat) : EncodeGraphM SerialValue := do
  -- Encode a SerialValue, replacing shared values with references.
  match sv with
  | .str _ | .nat _ | .bool _ | .ref _ => return sv
  | .compound name children =>
    if shared.getD sv 0 > 1 then
      let state ← get
      match state.seen.get? sv with
      | some id =>
        return .ref id
      | none =>
        let newId := state.nextId
        modify fun s => { s with
          seen := s.seen.insert sv newId,
          nextId := newId + 1,
          objects := s.objects.push sv
        }
        return .ref newId
    else
      let children' ← children.mapM (encodeWithRefs · shared)
      return .compound name children'

def encodeGraph {α : Type} [Serializable α] (obj : α) : IO GraphData := do
  let initialSv ← encode obj
  let counts := countOccurrences initialSv {}
  let sharedValues := counts.filter (fun _ count => count > 1)
  let (root, state) := (encodeWithRefs initialSv sharedValues).run EncodeState.empty
  return ⟨root, state.objects⟩

partial def decodeGraph {α : Type} [Serializable α] (gd : GraphData) : DecodeM α := do
  -- First, resolve all refs in the entire structure to get a ref-free tree
  -- Then decode the root value
  let rec resolveRefs (sv : SerialValue) : IO (Except String SerialValue) :=
    match sv with
    | .ref id =>
      if h : id < gd.objects.size then
        resolveRefs gd.objects[id]
      else
        return .error s!"Reference {id} out of bounds"
    | .compound name children => do
      let mut resolved : Array SerialValue := #[]
      for child in children do
        match ← resolveRefs child with
        | .ok childResolved => resolved := resolved.push childResolved
        | .error e => return .error e
      return .ok (.compound name resolved)
    | other => return .ok other

  let resolvedRoot ← resolveRefs gd.root
  decode resolvedRoot

def decodeCompound (expectedName : String) (sv : SerialValue) : DecodeM (Array SerialValue) := do
  match sv with
  | .compound name args =>
    if name == expectedName then return args
    else throw s!"Expected {expectedName}, got {name}"
  | other => throw s!"Expected compound {expectedName}, got {repr other}"


end LeanSerde
