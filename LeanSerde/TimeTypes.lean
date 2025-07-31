import Std.Time
import LeanSerde.Core

namespace LeanSerde

-- Units: Era, Year, Month, Week, Weekday, Day, Hour, Minute, Second, Millisecond, Nanosecond
instance : Serializable Std.Time.Year.Era where
  encode era := return (.compound "Era" #[.bool (match era with | .bce => true | .ce => false)])
  decode sv := do
    let args ← decodeCompound "Era" sv
    if args.size = 1 then
      match args[0]! with
      | .bool isBce => return (if isBce then Std.Time.Year.Era.bce else Std.Time.Year.Era.ce)
      | other => throw s!"Expected Bool for Era, got {repr other}"
    else
      throw s!"Era expects 1 arg, got {args.size}"

instance : Serializable Std.Time.Year.Offset where
  encode offset := return (.compound "YearOffset" #[.nat offset.toInt.natAbs, .bool (offset.toInt >= 0)])
  decode sv := do
    let args ← decodeCompound "YearOffset" sv
    if args.size = 2 then
      match args[0]!, args[1]! with
      | .nat offsetAbs, .bool isPositive =>
        let offsetVal := if isPositive then Int.ofNat offsetAbs else -Int.ofNat offsetAbs
        return (Std.Time.Year.Offset.ofInt offsetVal)
      | _, _ => throw "Expected (Nat, Bool) for Year.Offset"
    else
      throw s!"Year.Offset expects 2 args, got {args.size}"

def serializableBoundedOrdinal
  (α : Type) (name : String) (lo hi : Nat)
  (toNat : α → Nat) (ofNat : (n : Nat) → (h : n ≥ lo ∧ n ≤ hi) → α)
  : Serializable α where
  encode ord := return (.compound name #[.nat (toNat ord)])
  decode sv := do
    let args ← decodeCompound name sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        if h : n ≥ lo ∧ n ≤ hi then
          return (ofNat n h)
        else
          throw s!"{name} must be between {lo} and {hi}, got {n}"
      | other => throw s!"Expected Nat for {name}, got {repr other}"
    else
      throw s!"{name} expects 1 arg, got {args.size}"

def serializableUpperBoundedOrdinal
  (α : Type) (name : String) (hi : Nat)
  (toNat : α → Nat) (ofNat : (n : Nat) → (h : n ≤ hi) → α)
  : Serializable α where
  encode ord := return (.compound name #[.nat (toNat ord)])
  decode sv := do
    let args ← decodeCompound name sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        if h : n ≤ hi then
          return (ofNat n h)
        else
          throw s!"{name} must be between 0 and {hi}, got {n}"
      | other => throw s!"Expected Nat for {name}, got {repr other}"
    else
      throw s!"{name} expects 1 arg, got {args.size}"

instance : Serializable (Std.Time.Second.Ordinal leap) where
  encode o := return (.compound "SecondOrdinal" #[.nat o.val.natAbs])
  decode sv := do
    let args ← decodeCompound "SecondOrdinal" sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        let hi := if leap then 60 else 59
        if h : n ≤ hi then
          return (Std.Time.Second.Ordinal.ofNat n h)
        else
          throw s!"SecondOrdinal must be between 0 and {hi}, got {n}"
      | other => throw s!"Expected Nat for SecondOrdinal, got {repr other}"
    else
      throw s!"SecondOrdinal expects 1 arg, got {args.size}"

instance : Serializable Std.Time.Month.Ordinal :=
  serializableBoundedOrdinal
    Std.Time.Month.Ordinal "MonthOrdinal" 1 12
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Month.Ordinal.ofNat n h)

instance : Serializable Std.Time.Week.Ordinal :=
  serializableBoundedOrdinal
    Std.Time.Week.Ordinal "WeekOrdinal" 1 53
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Week.Ordinal.ofNat n h)

instance : Serializable Std.Time.Weekday :=
  serializableBoundedOrdinal
    Std.Time.Weekday "Weekday" 1 7
    (fun wd => wd.toOrdinal.val.natAbs)
    (fun n _ =>
      match Std.Time.Weekday.ofNat? n with
      | some wd => wd
      | none => panic! s!"Invalid Weekday ordinal: {n}"
    )

instance : Serializable Std.Time.Day.Ordinal :=
  serializableBoundedOrdinal
    Std.Time.Day.Ordinal "DayOrdinal" 1 31
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Day.Ordinal.ofNat n h)

instance : Serializable Std.Time.Hour.Ordinal :=
  serializableUpperBoundedOrdinal
    Std.Time.Hour.Ordinal "HourOrdinal" 23
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Hour.Ordinal.ofNat n h)

instance : Serializable Std.Time.Minute.Ordinal :=
  serializableUpperBoundedOrdinal
    Std.Time.Minute.Ordinal "MinuteOrdinal" 59
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Minute.Ordinal.ofNat n h)

instance : Serializable Std.Time.Millisecond.Ordinal :=
  serializableUpperBoundedOrdinal
    Std.Time.Millisecond.Ordinal "MillisecondOrdinal" 999
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Millisecond.Ordinal.ofNat n h)

instance : Serializable Std.Time.Nanosecond.Ordinal :=
  serializableUpperBoundedOrdinal
    Std.Time.Nanosecond.Ordinal "NanosecondOrdinal" 999_999_999
    (fun o => o.val.natAbs)
    (fun n h => Std.Time.Nanosecond.Ordinal.ofNat n h)

-- Timestamp
instance : Serializable Std.Time.Timestamp where
  encode ts := return (.compound "Timestamp" #[.nat ts.toSecondsSinceUnixEpoch.val.natAbs])
  decode sv := do
    let args ← decodeCompound "Timestamp" sv
    if args.size = 1 then
      match args[0]! with
      | .nat seconds => return (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨Int.ofNat seconds⟩)
      | other => throw s!"Expected Nat for timestamp seconds, got {repr other}"
    else
      throw "Timestamp expects 1 arg"

-- PlainDate, PlainTime, PlainDateTime
instance : Serializable Std.Time.PlainDate where
  encode date := do
    let year ← encode date.year
    let month ← encode date.month
    let day ← encode date.day
    return (.compound "PlainDate" #[year, month, day])
  decode sv := do
    let args ← decodeCompound "PlainDate" sv
    if args.size = 3 then
      let year ← decode args[0]!
      let month ← decode args[1]!
      let day ← decode args[2]!
      match Std.Time.PlainDate.ofYearMonthDay? year month day with
      | some validDate => return validDate
      | none => throw s!"Invalid date: year {year.toInt}, month {month.val}, day {day.val}"
    else
      throw s!"PlainDate expects 3 args, got {args.size}"

instance : Serializable Std.Time.PlainTime where
  encode time := do
    let hour ← encode time.hour
    let minute ← encode time.minute
    let second ← encode time.second
    let nanosecond ← encode time.nanosecond
    return (.compound "PlainTime" #[hour, minute, second, nanosecond])
  decode sv := do
    let args ← decodeCompound "PlainTime" sv
    if args.size = 4 then
      let hour ← decode args[0]!
      let minute ← decode args[1]!
      let second ← decode args[2]!
      let nanosecond ← decode args[3]!
      return (Std.Time.PlainTime.ofHourMinuteSecondsNano hour minute second nanosecond)
    else
      throw s!"PlainTime expects 4 args, got {args.size}"

instance : Serializable Std.Time.PlainDateTime where
  encode dateTime := do
    let date ← encode dateTime.date
    let time ← encode dateTime.time
    return (.compound "PlainDateTime" #[date, time])
  decode sv := do
    let args ← decodeCompound "PlainDateTime" sv
    if args.size = 2 then
      let date ← decode args[0]!
      let time ← decode args[1]!
      return { date := date, time := time }
    else
      throw s!"PlainDateTime expects 2 args, got {args.size}"

-- TimeZone
instance : Serializable Std.Time.TimeZone where
  encode tz := return (.compound "TimeZone" #[.str tz.name, .str tz.abbreviation, .nat tz.offset.second.val.natAbs, .bool (tz.offset.second.val >= 0), .bool tz.isDST])
  decode sv := do
    let args ← decodeCompound "TimeZone" sv
    if args.size = 5 then
      match args[0]!, args[1]!, args[2]!, args[3]!, args[4]! with
      | .str name, .str abbr, .nat offset, .bool isPositiveOffset, .bool isDST =>
        let offsetSeconds := if isPositiveOffset then Int.ofNat offset else -Int.ofNat offset
        return {
          name := name,
          abbreviation := abbr,
          offset := { second := { val := offsetSeconds } },
          isDST := isDST
        }
      | _, _, _, _, _ => throw "Expected TimeZone arguments to be (String, String, Nat, Bool, Bool)"
    else
      throw s!"TimeZone expects 5 args, got {args.size}"

-- ZoneRules serialization
instance : Serializable Std.Time.TimeZone.ZoneRules where
  encode zr := do
    let encodeLocalTimeType (ltt : Std.Time.TimeZone.LocalTimeType) : Array SerialValue :=
      #[.nat ltt.gmtOffset.second.val.natAbs, .bool (ltt.gmtOffset.second.val >= 0),
        .bool ltt.isDst, .str ltt.abbreviation,
        .bool (match ltt.wall with | .wall => true | .standard => false),
        .bool (match ltt.utLocal with | .ut => true | .local => false),
        .str ltt.identifier]

    let encodeTransition (t : Std.Time.TimeZone.Transition) : Array SerialValue :=
      #[.nat t.time.val.natAbs, .bool (t.time.val >= 0)] ++ encodeLocalTimeType t.localTimeType

    return (.compound "ZoneRules" (encodeLocalTimeType zr.initialLocalTimeType ++
      #[.compound "Transitions" (zr.transitions.map (fun t => .compound "T" (encodeTransition t)))]))

  decode sv := do
    let args ← decodeCompound "ZoneRules" sv
    if args.size >= 8 then
      let decodeLocalTimeType (offset: Nat) : DecodeM Std.Time.TimeZone.LocalTimeType := do
        match args[offset]!, args[offset+1]!, args[offset+2]!, args[offset+3]!, args[offset+4]!, args[offset+5]!, args[offset+6]! with
        | .nat gmtOff, .bool isPos, .bool isDst, .str abbr, .bool isWall, .bool isUt, .str id =>
          return {
            gmtOffset := { second := { val := if isPos then Int.ofNat gmtOff else -Int.ofNat gmtOff } },
            isDst := isDst, abbreviation := abbr,
            wall := if isWall then .wall else .standard,
            utLocal := if isUt then .ut else .local,
            identifier := id
          }
        | _, _, _, _, _, _, _ => throw "Invalid LocalTimeType format"

      let initialLtt ← decodeLocalTimeType 0
      match args[7]! with
      | .compound "Transitions" transArray =>
        let transitions ← transArray.mapM fun t => do
          let tArgs ← decodeCompound "T" t
          if tArgs.size = 9 then
            match tArgs[0]!, tArgs[1]! with
            | .nat time, .bool isPos =>
              let ltt ← decodeLocalTimeType 2  -- Skip time fields
              return { time := { val := if isPos then Int.ofNat time else -Int.ofNat time }, localTimeType := ltt }
            | _, _ => throw "Invalid Transition time format"
          else throw "Transition expects 9 args"
        return { initialLocalTimeType := initialLtt, transitions := transitions }
      | _ => throw "Expected Transitions compound"
    else throw "ZoneRules expects at least 8 args"

-- ZonedDateTime serialization
instance : Serializable Std.Time.ZonedDateTime where
  encode zdt := do
    let timestamp ← encode zdt.timestamp
    let timezone ← encode zdt.timezone
    return (.compound "ZonedDateTime" #[timestamp, timezone])
  decode sv := do
    let args ← decodeCompound "ZonedDateTime" sv
    if args.size = 2 then
      let timestamp : Std.Time.Timestamp ← decode args[0]!
      let timezone : Std.Time.TimeZone ← decode args[1]!
      return (Std.Time.ZonedDateTime.ofTimestampWithZone timestamp timezone)
    else
      throw s!"ZonedDateTime expects 2 args, got {args.size}"

end LeanSerde
