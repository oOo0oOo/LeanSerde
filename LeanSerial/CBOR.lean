import LeanSerial.Core

namespace LeanSerial.CBOR

inductive MajorType : Type where
  | unsignedInt | negativeInt | byteString | textString
  | array | map | tag | simple
  deriving BEq, Repr

def MajorType.toByte : MajorType → UInt8
  | .unsignedInt => 0x00  | .negativeInt => 0x20
  | .byteString => 0x40   | .textString => 0x60
  | .array => 0x80        | .map => 0xA0
  | .tag => 0xC0          | .simple => 0xE0

def MajorType.fromByte (b : UInt8) : Option MajorType :=
  match b / 32 with
  | 0 => some .unsignedInt | 1 => some .negativeInt
  | 2 => some .byteString  | 3 => some .textString
  | 4 => some .array       | 5 => some .map
  | 6 => some .tag         | 7 => some .simple
  | _ => none

inductive SimpleValue : Type where
  | false | true | null
  deriving BEq, Repr

def SimpleValue.toByte : SimpleValue → UInt8
  | .false => 20  | .true => 21  | .null => 22

def natToBytesBE (n : Nat) (bytes : Nat) : Array UInt8 :=
  Array.range bytes |>.map (fun i => ((n >>> ((bytes - 1 - i) * 8)) % 256).toUInt8)

def encodeLength (majorType : MajorType) (length : Nat) : ByteArray :=
  let base := majorType.toByte
  if length < 24 then
    ⟨#[base + length.toUInt8]⟩
  else if length < 256 then
    ⟨#[base + 24, length.toUInt8]⟩
  else if length < 65536 then
    ⟨#[base + 25] ++ natToBytesBE length 2⟩
  else if length < 4294967296 then
    ⟨#[base + 26] ++ natToBytesBE length 4⟩
  else
    ⟨#[base + 27] ++ natToBytesBE length 8⟩

-- Encoding functions
def encodeUnsignedInt : Nat → ByteArray := encodeLength .unsignedInt

def encodeBool (b : Bool) : ByteArray :=
  ⟨#[MajorType.simple.toByte + (if b then SimpleValue.true else SimpleValue.false).toByte]⟩

def encodeTextString (s : String) : ByteArray :=
  let utf8 := s.toUTF8
  encodeLength .textString utf8.size ++ utf8

def encodeArray (items : Array ByteArray) : ByteArray :=
  encodeLength .array items.size ++ items.foldl (· ++ ·) ⟨#[]⟩

def encodeMap (pairs : Array (ByteArray × ByteArray)) : ByteArray :=
  encodeLength .map pairs.size ++ pairs.foldl (fun acc (k, v) => acc ++ k ++ v) ⟨#[]⟩

partial def encodeSerialValue : SerialValue → ByteArray
  | .str s => encodeTextString s
  | .nat n => encodeUnsignedInt n
  | .bool b => encodeBool b
  | .compound "map" children =>
    let pairs := children.map fun child =>
      match child with
      | .compound "pair" #[.str k, v] => (encodeTextString k, encodeSerialValue v)
      | _ => (encodeTextString "", encodeSerialValue child) -- Should not happen
    encodeMap pairs
  | .compound name children =>
    encodeArray (#[encodeTextString name] ++ children.map encodeSerialValue)
  | .ref id =>
    encodeMap #[(encodeTextString "ref", encodeUnsignedInt id)]

-- Decoding state and helpers
structure DecodeState where
  data : ByteArray
  pos : Nat

def DecodeState.hasMore (s : DecodeState) : Bool := s.pos < s.data.size

def DecodeState.consume (s : DecodeState) (n : Nat) : Option (ByteArray × DecodeState) :=
  if s.pos + n <= s.data.size then
    some (s.data.extract s.pos (s.pos + n), ⟨s.data, s.pos + n⟩)
  else none

def DecodeState.consumeByte (s : DecodeState) : Option (UInt8 × DecodeState) :=
  s.consume 1 |>.map (fun (bytes, s') => (bytes[0]!, s'))

def bytesToNatBE (bytes : ByteArray) : Nat :=
  bytes.foldl (fun acc b => acc * 256 + b.toNat) 0

def decodeHeader (s : DecodeState) : Option (MajorType × Nat × Nat × DecodeState) := do
  let (firstByte, s') ← s.consumeByte
  let majorType ← MajorType.fromByte firstByte
  let additionalInfo := firstByte.toNat % 32
  if additionalInfo < 24 then
    some (majorType, additionalInfo, additionalInfo, s')
  else if additionalInfo == 24 then do
    let (b, s'') ← s'.consumeByte
    some (majorType, b.toNat, additionalInfo, s'')
  else if additionalInfo == 25 then do
    let (bytes, s'') ← s'.consume 2
    some (majorType, bytesToNatBE bytes, additionalInfo, s'')
  else if additionalInfo == 26 then do
    let (bytes, s'') ← s'.consume 4
    some (majorType, bytesToNatBE bytes, additionalInfo, s'')
  else if additionalInfo == 27 then do
    let (bytes, s'') ← s'.consume 8
    some (majorType, bytesToNatBE bytes, additionalInfo, s'')
  else
    none

partial def decodeSerialValue (s : DecodeState) : Option (SerialValue × DecodeState) := do
  let (majorType, len, additionalInfo, s') ← decodeHeader s

  match majorType with
  | .unsignedInt =>
    some (.nat len, s')

  | .textString => do
    let (bytes, s'') ← s'.consume len
    let str ← String.fromUTF8? bytes
    some (.str str, s'')

  | .array => do
    if len == 0 then
      some (.compound "" #[], s')
    else do
      let (nameVal, s'') ← decodeSerialValue s'
      let name ← match nameVal with | .str n => some n | _ => none
      let (children, finalState) ← decodeArrayElements (len - 1) #[] s''
      some (.compound name children, finalState)

  | .map => do
    let (pairs, finalState) ← decodeMapPairs len #[] s'
    if len == 1 then
      let (k, v) := pairs[0]!
      if k == "ref" then
        match v with
        | .nat id => some (.ref id, finalState)
        | _ => none -- Invalid ref format
      else
        let children := pairs.map fun (k, v) => .compound "pair" #[.str k, v]
        some (.compound "map" children, finalState)
    else
      let children := pairs.map fun (k, v) => .compound "pair" #[.str k, v]
      some (.compound "map" children, finalState)

  | .simple =>
    if additionalInfo == 20 then
      some (.bool false, s')
    else if additionalInfo == 21 then
      some (.bool true, s')
    else
      none -- null and other simple values not supported

  | .negativeInt | .byteString | .tag =>
    none -- These types are not used in SerialValue

where
  decodeArrayElements (remaining : Nat) (acc : Array SerialValue) (state : DecodeState)
    : Option (Array SerialValue × DecodeState) :=
    if remaining == 0 then
      some (acc, state)
    else do
      let (val, newState) ← decodeSerialValue state
      decodeArrayElements (remaining - 1) (acc.push val) newState

  decodeMapPairs (remaining : Nat) (acc : Array (String × SerialValue)) (state : DecodeState)
    : Option (Array (String × SerialValue) × DecodeState) :=
    if remaining == 0 then
      some (acc, state)
    else do
      let (keyVal, state') ← decodeSerialValue state
      let (valueVal, state'') ← decodeSerialValue state'
      match keyVal with
      | .str key =>
        decodeMapPairs (remaining - 1) (acc.push (key, valueVal)) state''
      | _ => none

def encodeGraphToCBOR (gd : GraphData) : ByteArray :=
  let rootEncoded := encodeSerialValue gd.root
  let objectsArray := encodeArray (gd.objects.map encodeSerialValue)
  encodeMap #[
    (encodeTextString "root", rootEncoded),
    (encodeTextString "objects", objectsArray)
  ]

partial def decodeArrayBody (remaining : Nat) (acc : Array SerialValue) (state : DecodeState)
  : Option (Array SerialValue × DecodeState) :=
  if remaining == 0 then
    some (acc, state)
  else do
    let (val, newState) ← decodeSerialValue state
    decodeArrayBody (remaining - 1) (acc.push val) newState

def decodeGraphFromCBOR (bytes : ByteArray) : Except String GraphData := do
  let lift {α} (o : Option α) (e : String) : Except String α :=
    match o with
    | some a => .ok a
    | none => .error e

  if bytes.isEmpty then
    throw "Empty ByteArray"

  let s := ⟨bytes, 0⟩
  let (majorType, mapLen, _, s') ← lift (decodeHeader s) "Failed to decode GraphData header"

  if majorType != .map then
    throw s!"Expected CBOR map for GraphData, but got major type: {repr majorType}"

  if mapLen != 2 then
    throw s!"Expected GraphData map to have 2 pairs, got {mapLen}"

  let (key1, s'') ← lift (decodeSerialValue s') "Failed to decode 'root' key"
  let .str "root" := key1 | throw "Expected 'root' key first"
  let (rootVal, s''') ← lift (decodeSerialValue s'') "Failed to decode 'root' value"

  let (key2, s'''') ← lift (decodeSerialValue s''') "Failed to decode 'objects' key"
  let .str "objects" := key2 | throw "Expected 'objects' key second"

  let (objMajorType, objLen, _, s_obj) ← lift (decodeHeader s'''') "Failed to decode objects array header"

  if objMajorType != .array then
    throw "Expected 'objects' field to be a CBOR array"

  let (objects, _) ← lift (decodeArrayBody objLen #[] s_obj) "Failed to decode content of objects array"

  .ok ⟨rootVal, objects⟩

instance : SerializableFormat ByteArray where
  serializeValue := encodeGraphToCBOR
  deserializeValue := decodeGraphFromCBOR

end LeanSerial.CBOR
