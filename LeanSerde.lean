import LeanSerde.Core
import LeanSerde.CBOR
import LeanSerde.Derive
import LeanSerde.PrimitiveTypes
import LeanSerde.ContainerTypes
import LeanSerde.LibraryTypes
import LeanSerde.TimeTypes
import LeanSerde.Describe

namespace LeanSerde

@[inline]
def serialize {α β m} [MonadLiftT IO m] [Serializable α] [SerializableFormat β] (value : α) : m β :=
  liftM (do
    let graphData ← encodeGraph value
    return SerializableFormat.serializeValue graphData)

@[inline]
def deserialize {α β} {m : Type → Type} [MonadLiftT IO m] [Serializable α] [SerializableFormat β] (data : β) : m (Except String α) :=
  liftM (do
    match SerializableFormat.deserializeValue data with
    | .error e => return .error e
    | .ok graphData => decodeGraph graphData : IO (Except String α))

def serializeToFile {α} [Serializable α] (value : α) (filePath : String) : IO Unit := do
  let data : ByteArray ← serialize value
  IO.FS.writeBinFile filePath data

def deserializeFromFile {α} [Serializable α] (filePath : String) : IO (Except String α) := do
  let data ← IO.FS.readBinFile filePath
  deserialize data

def serializeToJsonFile {α} [Serializable α] (value : α) (filePath : String) : IO Unit := do
  let data : String ← serialize value
  IO.FS.writeFile filePath data

def deserializeFromJsonFile {α} [Serializable α] (filePath : String) : IO (Except String α) := do
  let data ← IO.FS.readFile filePath
  deserialize data

end LeanSerde
