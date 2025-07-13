import LeanSerial.Derive
import LeanSerial.Serializable

namespace LeanSerial

def serialize {α β} [Serializable α] [SerializableFormat β] (value : α) : β :=
  SerializableFormat.serializeValue (encode value)

def deserialize {α β} [Serializable α] [SerializableFormat β] (data : β) : Except String α :=
  SerializableFormat.deserializeValue data >>= decode

end LeanSerial
