import LeanSerial.Core
import LeanSerial.CBOR
import LeanSerial.Derive
import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes
import LeanSerial.LibraryTypes
import LeanSerial.TimeTypes
import LeanSerial.MetaTypes

namespace LeanSerial

def serialize {α β} [Serializable α] [SerializableFormat β] (value : α) : β :=
  SerializableFormat.serializeValue (encodeGraph value)

def deserialize {α β} [Serializable α] [SerializableFormat β] (data : β) : Except String α := do
  let graphData ← SerializableFormat.deserializeValue data
  decodeGraph graphData

def serializeToFile {α} [Serializable α] (value : α) (filePath : String) : IO Unit :=
  IO.FS.writeBinFile filePath (serialize value : ByteArray)

def deserializeFromFile {α} [Serializable α] (filePath : String) : IO (Except String α) := do
  deserialize <$> IO.FS.readBinFile filePath

def serializeToJsonFile {α} [Serializable α] (value : α) (filePath : String) : IO Unit :=
  IO.FS.writeFile filePath (serialize value : String)

def deserializeFromJsonFile {α} [Serializable α] (filePath : String) : IO (Except String α) := do
  deserialize <$> IO.FS.readFile filePath

end LeanSerial
