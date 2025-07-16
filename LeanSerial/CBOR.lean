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

partial def encodeSerialValue : SerialValue → ByteArray
  | .str s => encodeTextString s
  | .nat n => encodeUnsignedInt n
  | .bool b => encodeBool b
  | .compound name children =>
    encodeArray (#[encodeTextString name] ++ children.map encodeSerialValue)

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

def decodeLength (s : DecodeState) : Option (Nat × DecodeState) := do
  let (firstByte, s') ← s.consumeByte
  let additionalInfo := firstByte.toNat % 32
  if additionalInfo < 24 then
    some (additionalInfo, s')
  else if additionalInfo == 24 then do
    let (b, s'') ← s'.consumeByte
    some (b.toNat, s'')
  else if additionalInfo == 25 then do
    let (bytes, s'') ← s'.consume 2
    some (bytesToNatBE bytes, s'')
  else if additionalInfo == 26 then do
    let (bytes, s'') ← s'.consume 4
    some (bytesToNatBE bytes, s'')
  else if additionalInfo == 27 then do
    let (bytes, s'') ← s'.consume 8
    some (bytesToNatBE bytes, s'')
  else
    none

partial def decodeSerialValue (s : DecodeState) : Option (SerialValue × DecodeState) := do
  let (firstByte, s') ← s.consumeByte
  let majorType ← MajorType.fromByte firstByte

  match majorType with
  | .unsignedInt => do
    let (n, s'') ← decodeLength ⟨s.data, s.pos⟩
    some (.nat n, s'')

  | .textString => do
    let (len, s'') ← decodeLength ⟨s.data, s.pos⟩
    let (bytes, s''') ← s''.consume len
    let str ← String.fromUTF8? bytes
    some (.str str, s''')

  | .array => do
    let (arrayLen, s'') ← decodeLength ⟨s.data, s.pos⟩
    if arrayLen == 0 then
      none
    else do
      let (nameVal, s''') ← decodeSerialValue s''
      let name ← match nameVal with | .str n => some n | _ => none
      let (children, finalState) ← decodeArrayElements (arrayLen - 1) #[] s'''
      some (.compound name children, finalState)

  | .simple =>
    let additionalInfo := firstByte.toNat % 32
    if additionalInfo == 20 then
      some (.bool false, s')
    else if additionalInfo == 21 then
      some (.bool true, s')
    else
      none

  | _ => none

where
  decodeArrayElements (remaining : Nat) (acc : Array SerialValue) (state : DecodeState)
    : Option (Array SerialValue × DecodeState) :=
    if remaining == 0 then
      some (acc, state)
    else do
      let (val, newState) ← decodeSerialValue state
      decodeArrayElements (remaining - 1) (acc.push val) newState

def encodeToCBOR : SerialValue → ByteArray := encodeSerialValue

def decodeFromCBOR (bytes : ByteArray) : Except String SerialValue :=
  match decodeSerialValue ⟨bytes, 0⟩ with
  | some (val, _) => .ok val
  | none => .error "Failed to decode CBOR data"

instance : SerializableFormat ByteArray where
  serializeValue := encodeToCBOR
  deserializeValue := decodeFromCBOR

end LeanSerial.CBOR
