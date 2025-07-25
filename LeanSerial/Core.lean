import Lean.Data.Json
import Std.Data.HashMap

namespace LeanSerial

inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  | ref : Nat → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

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
  seen : Std.HashMap USize Nat
  nextId : Nat
  objects : Array SerialValue

private def EncodeState.empty : EncodeState := ⟨Std.HashMap.ofList [], 0, #[]⟩

private abbrev EncodeM := StateM EncodeState
abbrev DecodeM := Except String

@[extern "lean_ptr_addr"]
private opaque getObjectId (a : α) : USize

class SerializableFormat (α : Type) where
  serializeValue : GraphData → α
  deserializeValue : α → Except String GraphData

instance : SerializableFormat Lean.Json where
  serializeValue := Lean.toJson
  deserializeValue := Lean.fromJson?

instance : SerializableFormat String where
  serializeValue gd := (Lean.toJson gd).pretty
  deserializeValue str := Lean.Json.parse str >>= Lean.fromJson?

class Serializable (α : Type) where
  encode : α → SerialValue
  decode : SerialValue → DecodeM α

export Serializable (encode decode)

private def encodeWithSharing {α : Type} [Serializable α] (obj : α) : EncodeM SerialValue := do
  let objId := getObjectId obj
  let state ← get
  match state.seen.get? objId with
  | some id => return .ref id
  | none =>
    let serialized := encode obj
    let newId := state.nextId
    modify fun s => { seen := s.seen.insert objId newId, nextId := newId + 1, objects := s.objects.push serialized }
    return serialized

def encodeGraph {α : Type} [Serializable α] (obj : α) : GraphData :=
  let (root, state) := (encodeWithSharing obj).run EncodeState.empty
  ⟨root, state.objects⟩

partial def decodeGraph {α : Type} [Serializable α] (gd : GraphData) : DecodeM α :=
  let rec resolve (sv : SerialValue) : DecodeM α :=
    match sv with
    | .ref id =>
      if h : id < gd.objects.size then resolve gd.objects[id]
      else .error s!"Reference {id} out of bounds"
    | other => decode other
  resolve gd.root

def decodeCompound (expectedName : String) (sv : SerialValue) : DecodeM (Array SerialValue) :=
  match sv with
  | .compound name args =>
    if name == expectedName then .ok args
    else .error s!"Expected {expectedName}, got {name}"
  | other => .error s!"Expected compound {expectedName}, got {repr other}"

end LeanSerial
