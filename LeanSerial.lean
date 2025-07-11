import Lean
import LeanSerial.ToExpr
import LeanSerial.FromExpr
import LeanSerial.Derive
import LeanSerial.Instances
import LeanSerial.Core

open Lean Elab Meta Term Command

namespace LeanSerial

def serialize {α} [Serialization.ToExpr α] (a : α) : IO ByteArray := do
  try
    let env ← importModules #[] {}
    let (bytes, _) ← (serializeCore a).toIO { fileName := "<serialize>", fileMap := default } { env := env, ngen := default }
    pure bytes
  catch e =>
    throw (IO.userError (toString e))

def deserialize {α} [Serialization.FromExpr α] (bytes : ByteArray) : IO α := do
  try
    let env ← importModules #[] {}
    let (a, _) ← (deserializeCore bytes).toIO { fileName := "<deserialize>", fileMap := default } { env := env, ngen := default }
    pure a
  catch e =>
    throw (IO.userError (toString e))

end LeanSerial
