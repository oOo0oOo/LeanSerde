import LeanSerial.Core
import LeanSerial.Derive
import LeanSerial.PrimitiveTypes
import LeanSerial.ContainerTypes
import LeanSerial.LibraryTypes
import LeanSerial.TimeTypes
import LeanSerial.MetaTypes

namespace LeanSerial

def serialize {α β} [Serializable α] [SerializableFormat β] (value : α) : β :=
  SerializableFormat.serializeValue (encode value)

def deserialize {α β} [Serializable α] [SerializableFormat β] (data : β) : Except String α :=
  SerializableFormat.deserializeValue data >>= decode

def serializeToJsonFile {α} [Serializable α] (value : α) (filePath : String) : IO Unit :=
  IO.FS.writeFile filePath (serialize value)

def deserializeFromJsonFile {α} [Serializable α] (filePath : String) : IO (Except String α) := do
  let jsonString ← IO.FS.readFile filePath
  return deserialize jsonString

end LeanSerial
