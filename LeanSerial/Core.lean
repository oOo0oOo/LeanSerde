import Lean

namespace LeanSerial

/--
Simplified serialization format with unified structure.
Primitives have no type witnesses for efficiency.
-/
inductive SerialValue where
  | str : String → SerialValue
  | nat : Nat → SerialValue
  | bool : Bool → SerialValue
  | compound : String → Array SerialValue → SerialValue
  deriving Repr, BEq, Inhabited

namespace SerialValue

/-- Convert SerialValue to string representation -/
def toString : SerialValue → String
  | str s => s!"\"{s}\""
  | nat n => Nat.repr n
  | bool b => if b then "true" else "false"
  | compound name children =>
  let childrenStr := String.intercalate "," (children.map toString).toList
  s!"\{\"type\":\"{name}\",\"args\":[{childrenStr}]}"

mutual
  private partial def parseArgs (s : String) : Except String (List SerialValue) := do
    let mut args := []
    let mut current := ""
    let mut nesting := 0
    let mut inString := false
    for c in s.toList do
      if c == '\"' then
        inString := not inString
        current := current.push c
      else if not inString && c == '{' then
        nesting := nesting + 1
        current := current.push c
      else if not inString && c == '}' then
        nesting := nesting - 1
        current := current.push c
      else if not inString && nesting == 0 && c == ',' then
        if current.isEmpty then
          -- allow for trailing comma
          continue
        let arg ← fromString current
        args := arg :: args
        current := ""
      else
        current := current.push c
    if !current.isEmpty then
      let arg ← fromString current
      args := arg :: args
    return args.reverse

  private partial def parseCompound (s : String) : Except String SerialValue := do
    if !s.startsWith "{\"type\":\"" || !s.endsWith "}" then
      throw s!"Invalid compound format: {s}"
    else
      -- Remove {"type":" and "}
      let content := s.drop 9 |>.dropRight 1

      -- Find where the type name ends (look for ","args":[)
      let typeEndPattern := "\",\"args\":["
      match content.splitOn typeEndPattern with
      | [typeName, argsContent] =>
        -- Remove the trailing ] from argsContent
        let argsString := argsContent.dropRight 1
        if argsString.isEmpty then
          return compound typeName #[]
        else
          let argsList ← parseArgs argsString
          return compound typeName argsList.toArray
      | _ => throw s!"Invalid compound format, could not parse: {s}"

  partial def fromString (s : String) : Except String SerialValue :=
    let s := s.trim
    if s.startsWith "\"" && s.endsWith "\"" then
      .ok (str (s.drop 1 |>.dropRight 1))
    else if s == "true" then
      .ok (bool true)
    else if s == "false" then
      .ok (bool false)
    else
      match s.toNat? with
      | some n => .ok (nat n)
      | none =>
        if s.startsWith "{\"type\":" then
          parseCompound s
        else
          .error s!"Invalid SerialValue format: {s}"
end

end SerialValue

def serializeValue (sv : SerialValue) : ByteArray :=
  sv.toString.toUTF8

def deserializeValue (bytes : ByteArray) : Except String SerialValue :=
  match String.fromUTF8? bytes with
  | none => .error "Invalid UTF-8 in serialized data"
  | some s => SerialValue.fromString s

end LeanSerial
