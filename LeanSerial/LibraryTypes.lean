import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Json
import Lean.Data.Position
import Lean.Data.RBMap
import Lean.Data.RBTree
import Lean.Data.PersistentHashMap
import Lean.Data.PersistentArray

import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes
import LeanSerial.Derive

namespace LeanSerial

-- HashMap
instance [Serializable k] [Serializable v] [BEq k] [Hashable k] : Serializable (Std.HashMap k v) where
  encode m := .compound "HashMap" ((m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v])).toArray)
  decode sv := do
    let args ← decodeCompound "HashMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (Std.HashMap.ofList entries.toList)

-- HashSet
instance [Serializable k] [BEq k] [Hashable k] : Serializable (Std.HashSet k) where
  encode s := .compound "HashSet" (s.toList.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "HashSet" sv
    let elems ← args.mapM decode |>.mapError (·)
    .ok (Std.HashSet.ofList elems.toList)

-- Lean.Json
instance : Serializable Lean.Json where
  encode (json : Lean.Json) := .compound "Json" #[encode (json.compress)]
  decode sv := do
    match sv with
    | .compound "Json" #[jsonStr] =>
      let jsonStr ← decode jsonStr
      match Lean.Json.parse jsonStr with
      | .ok json => .ok json
      | .error err => .error s!"Failed to parse JSON: {err}"
    | .compound "Json" args =>
      .error s!"Json expects 1 arg, got {args.size}"
    | other =>
      .error s!"Expected Json compound, got {repr other}"

-- Lean.Position
instance : Serializable Lean.Position where
  encode (pos : Lean.Position) := .compound "Position" #[.nat pos.line, .nat pos.column]
  decode sv := do
    match sv with
    | .compound "Position" #[.nat line, .nat col] =>
      if line < 0 || col < 0 then
        .error s!"Invalid Position: line {line}, col {col}"
      else
        .ok (Lean.Position.mk line col)
    | .compound "Position" args =>
      .error s!"Position expects 2 args, got {args.size}"
    | other =>
      .error s!"Expected Position compound, got {repr other}"

-- Lean.RBMap
instance {k v : Type} [Serializable k] [Serializable v] {cmp : k → k → Ordering} : Serializable (Lean.RBMap k v cmp) where
  encode m := .compound "RBMap" (m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v]) |>.toArray)
  decode sv := do
    let args ← decodeCompound "RBMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (Lean.RBMap.ofList entries.toList)

-- Lean.PersistentHashMap
instance {k v : Type} [Serializable k] [Serializable v] [BEq k] [Hashable k] : Serializable (Lean.PersistentHashMap k v) where
  encode m := .compound "PersistentHashMap" ((m.toList.map (fun ⟨k, v⟩ => .compound "Entry" #[encode k, encode v])).toArray)
  decode sv := do
    let args ← decodeCompound "PersistentHashMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          .ok (key, value)
      | _ => .error s!"Expected Entry compound, got {repr entry}")
    .ok (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) Lean.PersistentHashMap.empty)

-- Lean.PersistentArray
instance [Serializable α] : Serializable (Lean.PersistentArray α) where
  encode arr := .compound "PersistentArray" (arr.toList.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "PersistentArray" sv
    let elems ← args.mapM decode |>.mapError (·)
    if elems.isEmpty then
      .ok Lean.PersistentArray.empty
    else
      .ok (elems.foldl (init := Lean.PersistentArray.empty) (fun acc elem => acc.push elem))

-- Position and FileMap basics
instance : Serializable Lean.Position where
  encode pos := .compound "Position" #[.nat pos.line, .nat pos.column]
  decode sv := do
    match sv with
    | .compound "Position" #[.nat line, .nat col] =>
      .ok ⟨line, col⟩
    | .compound "Position" args =>
      .error s!"Position expects 2 args, got {args.size}"
    | other =>
      .error s!"Expected Position compound, got {repr other}"

-- Basic Format support
instance : Serializable Std.Format.FlattenBehavior where
  encode fb := match fb with
    | .allOrNone => .compound "FlattenBehavior.allOrNone" #[]
    | .fill => .compound "FlattenBehavior.fill" #[]
  decode sv := do
    match sv with
    | .compound "FlattenBehavior.allOrNone" #[] => .ok Std.Format.FlattenBehavior.allOrNone
    | .compound "FlattenBehavior.fill" #[] => .ok Std.Format.FlattenBehavior.fill
    | _ => .error s!"Expected FlattenBehavior compound, got {repr sv}"

-- RBTree
instance {k : Type} [Serializable k] {cmp : k → k → Ordering} : Serializable (Lean.RBTree k cmp) where
  encode t := .compound "RBTree" (t.toList.map encode |>.toArray)
  decode sv := do
    let args ← decodeCompound "RBTree" sv
    let elems ← args.mapM decode |>.mapError (·)
    .ok (elems.toList.foldl (init := Lean.RBTree.empty) (fun acc elem => acc.insert elem))

end LeanSerial
