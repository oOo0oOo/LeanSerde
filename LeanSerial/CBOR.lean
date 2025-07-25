import LeanSerial.Core

namespace LeanSerial.CBOR

private inductive MajorType : Type where
  | unsignedInt | negativeInt | byteString | textString
  | array | map | tag | simple
  deriving BEq, Repr

private def MajorType.toByte : MajorType → UInt8
  | .unsignedInt => 0x00  | .negativeInt => 0x20
  | .byteString => 0x40   | .textString => 0x60
  | .array => 0x80        | .map => 0xA0
  | .tag => 0xC0          | .simple => 0xE0

private def MajorType.fromByte (b : UInt8) : Option MajorType :=
  match b / 32 with
  | 0 => some .unsignedInt | 1 => some .negativeInt
  | 2 => some .byteString  | 3 => some .textString
  | 4 => some .array       | 5 => some .map
  | 6 => some .tag         | 7 => some .simple
  | _ => none

private def natToBytesBE (n : Nat) (bytes : Nat) : Array UInt8 :=
  Array.range bytes |>.map (fun i => ((n >>> ((bytes - 1 - i) * 8)) % 256).toUInt8)

private def encodeLength (majorType : MajorType) (length : Nat) : ByteArray :=
  let base := majorType.toByte
  if length < 24 then ⟨#[base + length.toUInt8]⟩
  else if length < 256 then ⟨#[base + 24, length.toUInt8]⟩
  else if length < 65536 then ⟨#[base + 25] ++ natToBytesBE length 2⟩
  else if length < 4294967296 then ⟨#[base + 26] ++ natToBytesBE length 4⟩
  else ⟨#[base + 27] ++ natToBytesBE length 8⟩

private def encodeUnsignedInt : Nat → ByteArray := encodeLength .unsignedInt

private def encodeBool (b : Bool) : ByteArray :=
  ⟨#[MajorType.simple.toByte + (if b then 21 else 20)]⟩

private def encodeTextString (s : String) : ByteArray :=
  let utf8 := s.toUTF8
  encodeLength .textString utf8.size ++ utf8

private def encodeArray (items : Array ByteArray) : ByteArray :=
  encodeLength .array items.size ++ items.foldl (· ++ ·) ⟨#[]⟩

private def encodeMap (pairs : Array (ByteArray × ByteArray)) : ByteArray :=
  encodeLength .map pairs.size ++ pairs.foldl (fun acc (k, v) => acc ++ k ++ v) ⟨#[]⟩

private partial def encodeSerialValue : SerialValue → ByteArray
  | .str s => encodeTextString s
  | .nat n => encodeUnsignedInt n
  | .bool b => encodeBool b
  | .compound "map" children =>
    let pairs := children.map fun child =>
      match child with
      | .compound "pair" #[.str k, v] => (encodeTextString k, encodeSerialValue v)
      | _ => (encodeTextString "", encodeSerialValue child)
    encodeMap pairs
  | .compound name children =>
    encodeArray (#[encodeTextString name] ++ children.map encodeSerialValue)
  | .ref id => encodeMap #[(encodeTextString "ref", encodeUnsignedInt id)]

private structure DecodeState where
  data : ByteArray
  pos : Nat

private def DecodeState.consume (s : DecodeState) (n : Nat) : Option (ByteArray × DecodeState) :=
  if s.pos + n <= s.data.size then
    some (s.data.extract s.pos (s.pos + n), ⟨s.data, s.pos + n⟩)
  else none

private def DecodeState.consumeByte (s : DecodeState) : Option (UInt8 × DecodeState) :=
  s.consume 1 |>.map (fun (bytes, s') => (bytes[0]!, s'))

private def bytesToNatBE (bytes : ByteArray) : Nat :=
  bytes.foldl (fun acc b => acc * 256 + b.toNat) 0

private def decodeHeader (s : DecodeState) : Option (MajorType × Nat × Nat × DecodeState) := do
  let (firstByte, s') ← s.consumeByte
  let majorType ← MajorType.fromByte firstByte
  let additionalInfo := firstByte.toNat % 32
  if additionalInfo < 24 then
    some (majorType, additionalInfo, additionalInfo, s')
  else if additionalInfo == 24 then do
    let (b, s'') ← s'.consumeByte
    some (majorType, b.toNat, additionalInfo, s'')
  else if additionalInfo ∈ [25, 26, 27] then do
    let byteCount := 2 ^ (additionalInfo - 24)
    let (bytes, s'') ← s'.consume byteCount
    some (majorType, bytesToNatBE bytes, additionalInfo, s'')
  else none

private partial def decodeSerialValue (s : DecodeState) : Option (SerialValue × DecodeState) := do
  let (majorType, len, additionalInfo, s') ← decodeHeader s
  match majorType with
  | .unsignedInt => some (.nat len, s')
  | .textString => do
    let (bytes, s'') ← s'.consume len
    let str ← String.fromUTF8? bytes
    some (.str str, s'')
  | .array =>
    if len == 0 then some (.compound "" #[], s')
    else do
      let (nameVal, s'') ← decodeSerialValue s'
      let .str name := nameVal | none
      let (children, finalState) ← decodeArrayElements (len - 1) #[] s''
      some (.compound name children, finalState)
  | .map => do
    let (pairs, finalState) ← decodeMapPairs len #[] s'
    if len == 1 then
      let (k, v) := pairs[0]!
      if k == "ref" then
        let .nat id := v | none
        some (.ref id, finalState)
      else
        let children := pairs.map fun (k, v) => .compound "pair" #[.str k, v]
        some (.compound "map" children, finalState)
    else
      let children := pairs.map fun (k, v) => .compound "pair" #[.str k, v]
      some (.compound "map" children, finalState)
  | .simple =>
    if additionalInfo == 20 then some (.bool false, s')
    else if additionalInfo == 21 then some (.bool true, s')
    else none
  | _ => none
where
  decodeArrayElements (remaining : Nat) (acc : Array SerialValue) (state : DecodeState)
    : Option (Array SerialValue × DecodeState) :=
    if remaining == 0 then some (acc, state)
    else do
      let (val, newState) ← decodeSerialValue state
      decodeArrayElements (remaining - 1) (acc.push val) newState

  decodeMapPairs (remaining : Nat) (acc : Array (String × SerialValue)) (state : DecodeState)
    : Option (Array (String × SerialValue) × DecodeState) :=
    if remaining == 0 then some (acc, state)
    else do
      let (keyVal, state') ← decodeSerialValue state
      let (valueVal, state'') ← decodeSerialValue state'
      let .str key := keyVal | none
      decodeMapPairs (remaining - 1) (acc.push (key, valueVal)) state''

private def encodeGraphToCBOR (gd : GraphData) : ByteArray :=
  encodeMap #[
    (encodeTextString "root", encodeSerialValue gd.root),
    (encodeTextString "objects", encodeArray (gd.objects.map encodeSerialValue))
  ]

private def decodeGraphFromCBOR (bytes : ByteArray) : Except String GraphData := do
  if bytes.isEmpty then throw "Empty ByteArray"

  let s := ⟨bytes, 0⟩
  let some (majorType, mapLen, _, s') := decodeHeader s | throw "Failed to decode header"
  if majorType != .map then throw s!"Expected CBOR map, got {repr majorType}"
  if mapLen != 2 then throw s!"Expected 2 pairs, got {mapLen}"

  let some (.str "root", s'') := decodeSerialValue s' | throw "Expected 'root' key"
  let some (rootVal, s''') := decodeSerialValue s'' | throw "Failed to decode root value"
  let some (.str "objects", s'''') := decodeSerialValue s''' | throw "Expected 'objects' key"
  let some (.array, objLen, _, s_obj) := decodeHeader s'''' | throw "Expected objects array"
  let some (objects, _) := decodeArrayElements objLen #[] s_obj | throw "Failed to decode objects"

  pure ⟨rootVal, objects⟩
where
  decodeArrayElements (remaining : Nat) (acc : Array SerialValue) (state : DecodeState)
    : Option (Array SerialValue × DecodeState) :=
    match remaining with
    | 0 => some (acc, state)
    | n + 1 => do
      let (val, newState) ← decodeSerialValue state
      decodeArrayElements n (acc.push val) newState

instance : SerializableFormat ByteArray where
  serializeValue := encodeGraphToCBOR
  deserializeValue := decodeGraphFromCBOR

end LeanSerial.CBOR
