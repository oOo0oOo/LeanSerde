import Tests.TestFramework
import LeanSerde
import Std.Time

open TestFramework

namespace TimeTests

def test_utc_zone_rules : IO TestResult := do
  let utcRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false)
  let bytes: ByteArray := LeanSerde.serialize utcRules
  match (LeanSerde.deserialize bytes : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => return TestResult.failure "ZoneRules UTC" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    let originalLtt := utcRules.initialLocalTimeType
    let deserializedLtt := deserialized.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       utcRules.transitions.size == deserialized.transitions.size then
      return TestResult.success "ZoneRules UTC"
    else
      return TestResult.failure "ZoneRules UTC" "Value mismatch"

def test_est_zone_rules : IO TestResult := do
  let estRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨-18000⟩ "EST" "EST" false)
  let bytes: ByteArray := LeanSerde.serialize estRules
  match (LeanSerde.deserialize bytes : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => return TestResult.failure "ZoneRules EST" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    let originalLtt := estRules.initialLocalTimeType
    let deserializedLtt := deserialized.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       estRules.transitions.size == deserialized.transitions.size then
      return TestResult.success "ZoneRules EST"
    else
      return TestResult.failure "ZoneRules EST" "Value mismatch"

def test_dst_zone_rules : IO TestResult := do
  let dstRules := Std.Time.TimeZone.ZoneRules.ofTimeZone (Std.Time.TimeZone.mk ⟨3600⟩ "CET" "CEST" true)
  let bytes: ByteArray := LeanSerde.serialize dstRules
  match (LeanSerde.deserialize bytes : Except String (Std.Time.TimeZone.ZoneRules)) with
  | .error e => return TestResult.failure "ZoneRules DST" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    let originalLtt := dstRules.initialLocalTimeType
    let deserializedLtt := deserialized.initialLocalTimeType
    if originalLtt.gmtOffset.second.val == deserializedLtt.gmtOffset.second.val &&
       originalLtt.isDst == deserializedLtt.isDst &&
       originalLtt.abbreviation == deserializedLtt.abbreviation &&
       originalLtt.identifier == deserializedLtt.identifier &&
       dstRules.transitions.size == deserialized.transitions.size then
      return TestResult.success "ZoneRules DST"
    else
      return TestResult.failure "ZoneRules DST" "Value mismatch"

def test_zoned_datetime_impl : IO TestResult := do
  let utcZone := Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false
  let zonedDateTime := Std.Time.ZonedDateTime.ofTimestampWithZone
    (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨2000000000⟩) utcZone
  let bytes: ByteArray := LeanSerde.serialize zonedDateTime
  match (LeanSerde.deserialize bytes : Except String Std.Time.ZonedDateTime) with
  | .error e => return TestResult.failure "ZonedDateTime" s!"Failed to deserialize: {e}"
  | .ok deserialized =>
    if deserialized.timestamp == zonedDateTime.timestamp &&
       deserialized.timezone == zonedDateTime.timezone then
      return TestResult.success "ZonedDateTime"
    else
      return TestResult.failure "ZonedDateTime" "Value mismatch"

def test_zoned_datetime : IO Unit := do
  let result ← test_zoned_datetime_impl
  if result.passed then
    IO.println "  ✓ ZonedDateTime"
  else
    IO.println s!"  ✗ ZonedDateTime: {result.error.getD "Unknown error"}"

def run: IO Bool := do
  let result1 ← runTests "Time Units" [
    test_roundtrip "Year" (Std.Time.Year.Offset.ofInt 2023),
    test_roundtrip "Month" (Std.Time.Month.Ordinal.ofNat 5),
    test_roundtrip "Week" (Std.Time.Week.Ordinal.ofNat 2),
    test_roundtrip "Weekday" (Std.Time.Weekday.ofNat 3),
    test_roundtrip "Day" (Std.Time.Day.Ordinal.ofNat 15),
    test_roundtrip "Hour" (Std.Time.Hour.Ordinal.ofNat 10 (by decide)),
    test_roundtrip "Minute" (Std.Time.Minute.Ordinal.ofNat 30 (by decide)),
    test_roundtrip "Second" (Std.Time.Second.Ordinal.ofNat (leap := false) 45 (by decide)),
    test_roundtrip "Second Leap" (Std.Time.Second.Ordinal.ofNat (leap := true) 60 (by decide)),
    test_roundtrip "Millisecond" (Std.Time.Millisecond.Ordinal.ofNat 500 (by decide)),
    test_roundtrip "Nanosecond" (Std.Time.Nanosecond.Ordinal.ofNat 999_999_999 (by decide))
  ]

  let result2 ← runTests "Time Structures" [
    test_roundtrip "Timestamp" (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨2000000000⟩),
    test_roundtrip "PlainDate" (Std.Time.PlainDate.ofYearMonthDay? 2023 5 15),
    test_roundtrip "PlainTime" (Std.Time.PlainTime.ofHourMinuteSeconds 10 30 45),
    test_roundtrip "TimeZone" (Std.Time.TimeZone.mk ⟨0⟩ "UTC" "UTC" false)
  ]

  let result3 ← runTests "Zone Rules" [
    test_utc_zone_rules,
    test_est_zone_rules,
    test_dst_zone_rules
  ]

  let result4 ← runTests "Complex DateTime" [
    test_zoned_datetime_impl
  ]

  -- Test PlainDateTime separately since it needs special handling
  let dateTime := Std.Time.PlainDate.ofYearMonthDay? 2023 5 15
  let dateTimeResult ← match dateTime with
  | none =>
    IO.println "  ✗ Failed to create PlainDate"
    pure false
  | some date => do
    let result ← test_roundtrip "PlainDateTime" (Std.Time.PlainDateTime.mk date (Std.Time.PlainTime.ofHourMinuteSeconds 10 30 45))
    if result.passed then
      IO.println "  ✓ PlainDateTime"
      pure true
    else
      IO.println s!"  ✗ PlainDateTime: {result.error.getD "Unknown error"}"
      pure false

  return result1 && result2 && result3 && result4 && dateTimeResult

end TimeTests
