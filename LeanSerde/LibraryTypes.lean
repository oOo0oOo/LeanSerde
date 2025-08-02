import Std.Data.HashMap
import Std.Data.HashSet
import Lean.Data.Json
import Lean.Data.Position
import Lean.Data.RBMap
import Lean.Data.RBTree
import Lean.Data.PersistentHashMap
import Lean.Data.PersistentArray

import LeanSerde.PrimitiveTypes
import LeanSerde.ContainerTypes
import LeanSerde.Derive

namespace LeanSerde
-- HashMap
instance [Serializable k] [Serializable v] [BEq k] [Hashable k] : Serializable (Std.HashMap k v) where
  encode m := do
    let entries ← m.toList.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "HashMap" entries.toArray
  decode sv := do
    let args ← decodeCompound "HashMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return (Std.HashMap.ofList entries.toList)

-- HashSet
instance [Serializable k] [BEq k] [Hashable k] : Serializable (Std.HashSet k) where
  encode s := do
    let encodedElems ← s.toList.mapM encode
    return .compound "HashSet" encodedElems.toArray
  decode sv := do
    let args ← decodeCompound "HashSet" sv
    let elems ← args.mapM decode
    return (Std.HashSet.ofList elems.toList)

-- Lean.Json
instance : Serializable Lean.Json where
  encode (json : Lean.Json) := do
    let encodedStr ← encode (json.compress)
    return .compound "Json" #[encodedStr]
  decode sv := do
    match sv with
    | .compound "Json" #[jsonStr] =>
      let jsonStr ← decode jsonStr
      match Lean.Json.parse jsonStr with
      | .ok json => return json
      | .error err => throw s!"Failed to parse JSON: {err}"
    | .compound "Json" args =>
      throw s!"Json expects 1 arg, got {args.size}"
    | other =>
      throw s!"Expected Json compound, got {repr other}"

-- Lean.RBMap
instance {k v : Type} [Serializable k] [Serializable v] {cmp : k → k → Ordering} : Serializable (Lean.RBMap k v cmp) where
  encode m := do
    let entries ← m.toList.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "RBMap" entries.toArray
  decode sv := do
    let args ← decodeCompound "RBMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return (Lean.RBMap.ofList entries.toList)

-- Lean.PersistentHashMap
instance {k v : Type} [Serializable k] [Serializable v] [BEq k] [Hashable k] : Serializable (Lean.PersistentHashMap k v) where
  encode m := do
    let entries ← m.toList.mapM (fun ⟨k, v⟩ => do
      let encodedK ← encode k
      let encodedV ← encode v
      return .compound "Entry" #[encodedK, encodedV])
    return .compound "PersistentHashMap" entries.toArray
  decode sv := do
    let args ← decodeCompound "PersistentHashMap" sv
    let entries ← args.mapM (fun entry => match entry with
      | .compound "Entry" #[k, v] =>
        do
          let key ← decode k
          let value ← decode v
          return (key, value)
      | _ => throw s!"Expected Entry compound, got {repr entry}")
    return (entries.toList.foldl (fun acc ⟨k, v⟩ => acc.insert k v) Lean.PersistentHashMap.empty)

-- Lean.PersistentArray
instance [Serializable α] : Serializable (Lean.PersistentArray α) where
  encode arr := do
    let encodedElems ← arr.toList.mapM encode
    return .compound "PersistentArray" encodedElems.toArray
  decode sv := do
    let args ← decodeCompound "PersistentArray" sv
    let elems ← args.mapM decode
    if elems.isEmpty then
      return Lean.PersistentArray.empty
    else
      return (elems.foldl (init := Lean.PersistentArray.empty) (fun acc elem => acc.push elem))

-- Position and FileMap basics
instance : Serializable Lean.Position where
  encode pos := return .compound "Position" #[.nat pos.line, .nat pos.column]
  decode sv := do
    match sv with
    | .compound "Position" #[.nat line, .nat col] =>
      return ⟨line, col⟩
    | .compound "Position" args =>
      throw s!"Position expects 2 args, got {args.size}"
    | other =>
      throw s!"Expected Position compound, got {repr other}"

-- RBTree
instance {k : Type} [Serializable k] {cmp : k → k → Ordering} : Serializable (Lean.RBTree k cmp) where
  encode t := do
    let encodedElems ← t.toList.mapM encode
    return .compound "RBTree" encodedElems.toArray
  decode sv := do
    let args ← decodeCompound "RBTree" sv
    let elems ← args.mapM decode
    return (elems.toList.foldl (init := Lean.RBTree.empty) (fun acc elem => acc.insert elem))

deriving instance LeanSerde.Serializable for Lean.Position, Std.Format.FlattenBehavior

end LeanSerde
