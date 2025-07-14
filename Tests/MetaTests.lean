import Lean
import Lean.Elab
import Lean.Meta

import Tests.TestFramework
import LeanSerial


open TestFramework Lean Elab Meta Term Command

namespace MetaTests

-- Name
def test_name : IO TestResult := do
  let name := Name.mkSimple "test.name"
  test_roundtrip "Name Serialization" name

def test_lmvar_id : IO TestResult := do
  let lmvarId : LMVarId := { name := Name.mkSimple "test.lmvar" }
  test_roundtrip "LMVarId Serialization" lmvarId

def test_level : IO TestResult := do
  let level : Level := Level.param (Name.mkSimple "test.level")
  test_roundtrip "Level Serialization" level

def test_sourceinfo : IO TestResult := do
  let sourceInfo := SourceInfo.synthetic ⟨10⟩ ⟨20⟩ false
  test_roundtrip "SourceInfo Serialization" sourceInfo

def run : IO Unit := do
  runTests "Meta Type Serialization" [
    test_name,
    test_lmvar_id,
    test_level,
    test_sourceinfo
  ]

end MetaTests
