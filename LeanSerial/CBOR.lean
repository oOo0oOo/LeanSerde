import LeanSerial.Core

namespace LeanSerial

-- CBOR Implementation for ByteArray
namespace CBOR

-- CBOR major types
def majorTypeUnsignedInt : UInt8 := 0x00
def majorTypeNegativeInt : UInt8 := 0x20
def majorTypeByteString : UInt8 := 0x40
def majorTypeTextString : UInt8 := 0x60
def majorTypeArray : UInt8 := 0x80
def majorTypeMap : UInt8 := 0xA0
def majorTypeTag : UInt8 := 0xC0
def majorTypeSimple : UInt8 := 0xE0

-- CBOR simple values
def simpleFalse : UInt8 := 20
def simpleTrue : UInt8 := 21
def simpleNull : UInt8 := 22

def encodeLength (majorType : UInt8) (length : Nat) : ByteArray :=
  if length < 24 then
    ⟨#[majorType + length.toUInt8]⟩
  else if length < 256 then
    ⟨#[majorType + 24, length.toUInt8]⟩
  else if length < 65536 then
    let high := (length / 256).toUInt8
    let low := (length % 256).toUInt8
    ⟨#[majorType + 25, high, low]⟩
  else if length < 4294967296 then
    let b3 := (length / 16777216).toUInt8
    let b2 := ((length / 65536) % 256).toUInt8
    let b1 := ((length / 256) % 256).toUInt8
    let b0 := (length % 256).toUInt8
    ⟨#[majorType + 26, b3, b2, b1, b0]⟩
  else
    -- For very large numbers, use 8-byte encoding
    let b7 := (length / 72057594037927936).toUInt8
    let b6 := ((length / 281474976710656) % 256).toUInt8
    let b5 := ((length / 1099511627776) % 256).toUInt8
    let b4 := ((length / 4294967296) % 256).toUInt8
    let b3 := ((length / 16777216) % 256).toUInt8
    let b2 := ((length / 65536) % 256).toUInt8
    let b1 := ((length / 256) % 256).toUInt8
    let b0 := (length % 256).toUInt8
    ⟨#[majorType + 27, b7, b6, b5, b4, b3, b2, b1, b0]⟩

def encodeUnsignedInt (n : Nat) : ByteArray :=
  encodeLength majorTypeUnsignedInt n

def encodeTextString (s : String) : ByteArray :=
  let utf8Bytes := s.toUTF8
  encodeLength majorTypeTextString utf8Bytes.size ++ utf8Bytes

def encodeBool (b : Bool) : ByteArray :=
  if b then ⟨#[majorTypeSimple + simpleTrue]⟩ else ⟨#[majorTypeSimple + simpleFalse]⟩

def encodeArray (items : Array ByteArray) : ByteArray :=
  let header := encodeLength majorTypeArray items.size
  items.foldl (· ++ ·) header

partial def encodeSerialValue (sv : SerialValue) : ByteArray :=
  match sv with
  | SerialValue.str s => encodeTextString s
  | SerialValue.nat n => encodeUnsignedInt n
  | SerialValue.bool b => encodeBool b
  | SerialValue.compound name children =>
    let encodedName := encodeTextString name
    let encodedChildren := children.map encodeSerialValue
    encodeArray (#[encodedName] ++ encodedChildren)

-- Decoder helpers
structure DecodeState where
  data : ByteArray
  pos : Nat

def DecodeState.hasMore (s : DecodeState) : Bool :=
  s.pos < s.data.size

def DecodeState.readByte (s : DecodeState) : Option (UInt8 × DecodeState) :=
  if s.pos < s.data.size then
    some (s.data[s.pos]!, { s with pos := s.pos + 1 })
  else
    none

def DecodeState.readBytes (s : DecodeState) (n : Nat) : Option (ByteArray × DecodeState) :=
  if s.pos + n <= s.data.size then
    let bytes := s.data.extract s.pos (s.pos + n)
    some (bytes, { s with pos := s.pos + n })
  else
    none

def decodeLength (s : DecodeState) : Option (Nat × DecodeState) :=
  match DecodeState.readByte s with
  | some (firstByte, s') =>
    let additionalInfo := firstByte.toNat % 32
    if additionalInfo < 24 then
      some (additionalInfo, s')
    else if additionalInfo == 24 then
      match DecodeState.readByte s' with
      | some (b, s'') => some (b.toNat, s'')
      | none => none
    else if additionalInfo == 25 then
      match DecodeState.readBytes s' 2 with
      | some (bytes, s'') =>
        let n := bytes[0]!.toNat * 256 + bytes[1]!.toNat
        some (n, s'')
      | none => none
    else if additionalInfo == 26 then
      match DecodeState.readBytes s' 4 with
      | some (bytes, s'') =>
        let n := bytes[0]!.toNat * 16777216 + bytes[1]!.toNat * 65536 +
                 bytes[2]!.toNat * 256 + bytes[3]!.toNat
        some (n, s'')
      | none => none
    else
      none -- Simplification: only support up to 32-bit lengths
  | none => none

partial def decodeSerialValue (s : DecodeState) : Option (SerialValue × DecodeState) :=
  match DecodeState.readByte s with
  | some (firstByte, s') =>
    let majorType := firstByte.toNat / 32
    if majorType == 0 then -- Unsigned integer
      let additionalInfo := firstByte.toNat % 32
      if additionalInfo < 24 then
        some (SerialValue.nat additionalInfo, s')
      else if additionalInfo == 24 then
        match DecodeState.readByte s' with
        | some (b, s'') => some (SerialValue.nat b.toNat, s'')
        | none => none
      else if additionalInfo == 25 then
        match DecodeState.readBytes s' 2 with
        | some (bytes, s'') =>
          let n := bytes[0]!.toNat * 256 + bytes[1]!.toNat
          some (SerialValue.nat n, s'')
        | none => none
      else
        none
    else if majorType == 3 then -- Text string
      match decodeLength ⟨s.data, s.pos⟩ with
      | some (len, s'') =>
        match DecodeState.readBytes s'' len with
        | some (bytes, s''') =>
          match String.fromUTF8? bytes with
          | some str => some (SerialValue.str str, s''')
          | none => none
        | none => none
      | none => none
    else if majorType == 4 then -- Array
      match decodeLength ⟨s.data, s.pos⟩ with
      | some (arrayLen, s'') =>
        if arrayLen == 0 then
          some (SerialValue.compound "" #[], s'')
        else
          -- First element should be the compound name
          match decodeSerialValue s'' with
          | some (nameVal, s''') =>
            match nameVal with
            | SerialValue.str name =>
              let rec decodeElements (remaining : Nat) (acc : Array SerialValue) (state : DecodeState) : Option (Array SerialValue × DecodeState) :=
                if remaining == 0 then some (acc, state)
                else
                  match decodeSerialValue state with
                  | some (val, newState) =>
                    decodeElements (remaining - 1) (acc.push val) newState
                  | none => none
              match decodeElements (arrayLen - 1) #[] s''' with
              | some (children, finalState) =>
                some (SerialValue.compound name children, finalState)
              | none => none
            | _ => none
          | none => none
      | none => none
    else if majorType == 7 then -- Simple/float
      let additionalInfo := firstByte.toNat % 32
      if additionalInfo == simpleFalse.toNat then
        some (SerialValue.bool false, s')
      else if additionalInfo == simpleTrue.toNat then
        some (SerialValue.bool true, s')
      else
        none
    else
      none
  | none => none

def encodeToCBOR (sv : SerialValue) : ByteArray :=
  encodeSerialValue sv

def decodeFromCBOR (bytes : ByteArray) : Except String SerialValue :=
  match decodeSerialValue ⟨bytes, 0⟩ with
  | some (val, _) => Except.ok val
  | none => Except.error "Failed to decode CBOR data"

end CBOR

instance : SerializableFormat ByteArray where
  serializeValue sv := CBOR.encodeToCBOR sv
  deserializeValue bytes := CBOR.decodeFromCBOR bytes

end LeanSerial
