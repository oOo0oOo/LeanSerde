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
    let some (Lean.Json.num id) := obj.get? "ref"
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

-- Preserve structural equality and wire ordering while hashing each subtree once.
private inductive HashedValue where
  | mk (value : SerialValue) (cachedHash : UInt64) (children : Array HashedValue)
  deriving Inhabited

private def HashedValue.value : HashedValue → SerialValue
  | .mk value _ _ => value
private def HashedValue.cachedHash : HashedValue → UInt64
  | .mk _ hash _ => hash
private def HashedValue.children : HashedValue → Array HashedValue
  | .mk _ _ children => children

private instance : Hashable HashedValue := ⟨HashedValue.cachedHash⟩
private instance : BEq HashedValue := ⟨fun a b => a.value == b.value⟩

private partial def hashTree (value : SerialValue) : HashedValue :=
  match value with
  | .compound name children =>
    let children := children.map hashTree
    let childrenHash := children.foldl (fun acc child => mixHash acc child.cachedHash) 7
    .mk value (mixHash 4 (mixHash (hash name) childrenHash)) children
  | _ => .mk value (hash value) #[]

private structure EncodeState where
  seen : Std.HashMap HashedValue Nat
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

private partial def countOccurrences (node : HashedValue)
    (counts : Std.HashMap HashedValue Nat) : Std.HashMap HashedValue Nat :=
  match node.value with
  | .compound _ _ =>
    let counts := counts.insert node (counts.getD node 0 + 1)
    node.children.foldl (fun acc child => countOccurrences child acc) counts
  | _ => counts

private partial def encodeWithRefs (node : HashedValue)
    (shared : Std.HashMap HashedValue Nat) : EncodeGraphM SerialValue := do
  match node.value with
  | .compound name _ =>
    if shared.getD node 0 > 1 then
      let state ← get
      match state.seen.get? node with
      | some id => return .ref id
      | none =>
        let newId := state.nextId
        modify fun s => { s with
          seen := s.seen.insert node newId,
          nextId := newId + 1,
          objects := s.objects.push node.value
        }
        return .ref newId
    else
      let children ← node.children.mapM (encodeWithRefs · shared)
      return .compound name children
  | other => return other

def encodeGraph {α : Type} [Serializable α] (obj : α) : IO GraphData := do
  let tree := hashTree (← encode obj)
  let counts := countOccurrences tree {}
  let sharedValues := counts.filter (fun _ count => count > 1)
  let (root, state) := (encodeWithRefs tree sharedValues).run EncodeState.empty
  return ⟨root, state.objects⟩

private structure ResolveState where
  active : Std.HashSet Nat := {}
  resolved : Std.HashMap Nat SerialValue := {}

private partial def resolveRefs (gd : GraphData) (sv : SerialValue) :
    ExceptT String (StateRefT ResolveState IO) SerialValue := do
  match sv with
  | .ref id =>
    if let some value := (← get).resolved[id]? then return value
    if (← get).active.contains id then throw s!"Cyclic reference {id}"
    if h : id < gd.objects.size then
      modify fun s => { s with active := s.active.insert id }
      let value ← resolveRefs gd gd.objects[id]
      modify fun s => { s with
        active := s.active.erase id,
        resolved := s.resolved.insert id value }
      return value
    else
      throw s!"Reference {id} out of bounds"
  | .compound name children =>
    return .compound name (← children.mapM (resolveRefs gd))
  | other => return other

partial def decodeGraph {α : Type} [Serializable α] (gd : GraphData) : DecodeM α := do
  let (result, _) ← (resolveRefs gd gd.root).run.run {}
  match result with
  | .error e => throw e
  | .ok root => decode root

def decodeCompound (expectedName : String) (sv : SerialValue) : DecodeM (Array SerialValue) := do
  match sv with
  | .compound name args =>
    if name == expectedName then return args
    else throw s!"Expected {expectedName}, got {name}"
  | other => throw s!"Expected compound {expectedName}, got {repr other}"


end LeanSerde
