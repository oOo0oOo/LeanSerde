import LeanSerial.Derive
import LeanSerial.Serializable

namespace LeanSerial

-- User-facing API for serialization and deserialization
def serialize {α} [Serializable α] (value : α) : ByteArray :=
  serializeValue (encode value)

def deserialize {α} [Serializable α] (bytes : ByteArray) : Except String α :=
  deserializeValue bytes >>= decode

end LeanSerial
