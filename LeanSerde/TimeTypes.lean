import Std.Time
import LeanSerde.Core

namespace LeanSerde

-- Units: Era, Year, Month, Week, Weekday, Day, Hour, Minute, Second, Millisecond, Nanosecond
instance : Serializable Std.Time.Year.Era where
  encode era := .compound "Era" #[.bool (match era with | .bce => true | .ce => false)]
  decode sv := do
    let args ← decodeCompound "Era" sv
    if args.size = 1 then
      match args[0]! with
      | .bool isBce => .ok (if isBce then Std.Time.Year.Era.bce else Std.Time.Year.Era.ce)
      | other => .error s!"Expected Bool for Era, got {repr other}"
    else
      .error s!"Era expects 1 arg, got {args.size}"

instance : Serializable Std.Time.Year.Offset where
  encode offset := .compound "YearOffset" #[.nat offset.toInt.natAbs, .bool (offset.toInt >= 0)]
  decode sv := do
    let args ← decodeCompound "YearOffset" sv
    if args.size = 2 then
      match args[0]!, args[1]! with
      | .nat offsetAbs, .bool isPositive =>
        let offsetVal := if isPositive then Int.ofNat offsetAbs else -Int.ofNat offsetAbs
        .ok (Std.Time.Year.Offset.ofInt offsetVal)
      | _, _ => .error "Expected (Nat, Bool) for Year.Offset"
    else
      .error s!"Year.Offset expects 2 args, got {args.size}"

def serializableBoundedOrdinal
  (α : Type) (name : String) (lo hi : Nat)
  (toNat : α → Nat) (ofNat : (n : Nat) → (h : n ≥ lo ∧ n ≤ hi) → α)
  : Serializable α where
  encode ord := .compound name #[.nat (toNat ord)]
  decode sv := do
    let args ← decodeCompound name sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        if h : n ≥ lo ∧ n ≤ hi then
          .ok (ofNat n h)
        else
          .error s!"{name} must be between {lo} and {hi}, got {n}"
      | other => .error s!"Expected Nat for {name}, got {repr other}"
    else
      .error s!"{name} expects 1 arg, got {args.size}"

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

def serializableUpperBoundedOrdinal
  (α : Type) (name : String) (hi : Nat)
  (toNat : α → Nat) (ofNat : (n : Nat) → (h : n ≤ hi) → α)
  : Serializable α where
  encode ord := .compound name #[.nat (toNat ord)]
  decode sv := do
    let args ← decodeCompound name sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        if h : n ≤ hi then
          .ok (ofNat n h)
        else
          .error s!"{name} must be between 0 and {hi}, got {n}"
      | other => .error s!"Expected Nat for {name}, got {repr other}"
    else
      .error s!"{name} expects 1 arg, got {args.size}"

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

instance : Serializable (Std.Time.Second.Ordinal leap) where
  encode o := .compound "SecondOrdinal" #[.nat o.val.natAbs]
  decode sv := do
    let args ← decodeCompound "SecondOrdinal" sv
    if args.size = 1 then
      match args[0]! with
      | .nat n =>
        let hi := if leap then 60 else 59
        if h : n ≤ hi then
          .ok (Std.Time.Second.Ordinal.ofNat n h)
        else
          .error s!"SecondOrdinal must be between 0 and {hi}, got {n}"
      | other => .error s!"Expected Nat for SecondOrdinal, got {repr other}"
    else
      .error s!"SecondOrdinal expects 1 arg, got {args.size}"

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
  encode ts := .compound "Timestamp" #[.nat ts.toSecondsSinceUnixEpoch.val.natAbs]
  decode sv := do
    let args ← decodeCompound "Timestamp" sv
    if args.size = 1 then do
      match args[0]! with
      | .nat seconds => .ok (Std.Time.Timestamp.ofSecondsSinceUnixEpoch ⟨Int.ofNat seconds⟩)
      | other => .error s!"Expected Nat for timestamp seconds, got {repr other}"
    else
      .error "Timestamp expects 1 arg"


-- PlainDate, PlainTime, PlainDateTime
instance : Serializable Std.Time.PlainDate where
  encode date := .compound "PlainDate" #[encode date.year, encode date.month, encode date.day]
  decode sv := do
    let args ← decodeCompound "PlainDate" sv
    if args.size = 3 then do
      let year ← decode args[0]!
      let month ← decode args[1]!
      let day ← decode args[2]!
      match Std.Time.PlainDate.ofYearMonthDay? year month day with
      | some validDate => .ok validDate
      | none => .error s!"Invalid date: year {year.toInt}, month {month.val}, day {day.val}"
    else
      .error s!"PlainDate expects 3 args, got {args.size}"

instance : Serializable Std.Time.PlainTime where
  encode time := .compound "PlainTime" #[
    encode time.hour,
    encode time.minute,
    encode time.second,
    encode time.nanosecond
  ]
  decode sv := do
    let args ← decodeCompound "PlainTime" sv
    if args.size = 4 then do
      let hour ← decode args[0]!
      let minute ← decode args[1]!
      let second ← decode args[2]!
      let nanosecond ← decode args[3]!
      .ok (Std.Time.PlainTime.ofHourMinuteSecondsNano hour minute second nanosecond)
    else
      .error s!"PlainTime expects 4 args, got {args.size}"

instance : Serializable Std.Time.PlainDateTime where
  encode dateTime := .compound "PlainDateTime" #[
    encode dateTime.date,
    encode dateTime.time
  ]
  decode sv := do
    let args ← decodeCompound "PlainDateTime" sv
    if args.size = 2 then do
      let date ← decode args[0]!
      let time ← decode args[1]!
      .ok { date := date, time := time }
    else
      .error s!"PlainDateTime expects 2 args, got {args.size}"

-- TimeZone
instance : Serializable Std.Time.TimeZone where
  encode tz := .compound "TimeZone" #[.str tz.name, .str tz.abbreviation, .nat tz.offset.second.val.natAbs, .bool (tz.offset.second.val >= 0), .bool tz.isDST]
  decode sv := do
    let args ← decodeCompound "TimeZone" sv
    if args.size = 5 then do
      match args[0]!, args[1]!, args[2]!, args[3]!, args[4]! with
      | .str name, .str abbr, .nat offset, .bool isPositiveOffset, .bool isDST =>
        let offsetSeconds := if isPositiveOffset then Int.ofNat offset else -Int.ofNat offset
        .ok {
          name := name,
          abbreviation := abbr,
          offset := { second := { val := offsetSeconds } },
          isDST := isDST
        }
      | _, _, _, _, _ => .error "Expected TimeZone arguments to be (String, String, Nat, Bool, Bool)"
    else
      .error s!"TimeZone expects 5 args, got {args.size}"

-- ZoneRules serialization
instance : Serializable Std.Time.TimeZone.ZoneRules where
  encode zr :=
    let encodeLocalTimeType (ltt : Std.Time.TimeZone.LocalTimeType) : Array SerialValue :=
      #[.nat ltt.gmtOffset.second.val.natAbs, .bool (ltt.gmtOffset.second.val >= 0),
        .bool ltt.isDst, .str ltt.abbreviation,
        .bool (match ltt.wall with | .wall => true | .standard => false),
        .bool (match ltt.utLocal with | .ut => true | .local => false),
        .str ltt.identifier]

    let encodeTransition (t : Std.Time.TimeZone.Transition) : Array SerialValue :=
      #[.nat t.time.val.natAbs, .bool (t.time.val >= 0)] ++ encodeLocalTimeType t.localTimeType

    .compound "ZoneRules" (encodeLocalTimeType zr.initialLocalTimeType ++
      #[.compound "Transitions" (zr.transitions.map (fun t => .compound "T" (encodeTransition t)))])

  decode sv := do
    let args ← decodeCompound "ZoneRules" sv
    if args.size >= 8 then do
      let decodeLocalTimeType (offset: Nat) : DecodeM Std.Time.TimeZone.LocalTimeType := do
        match args[offset]!, args[offset+1]!, args[offset+2]!, args[offset+3]!, args[offset+4]!, args[offset+5]!, args[offset+6]! with
        | .nat gmtOff, .bool isPos, .bool isDst, .str abbr, .bool isWall, .bool isUt, .str id =>
          .ok {
            gmtOffset := { second := { val := if isPos then Int.ofNat gmtOff else -Int.ofNat gmtOff } },
            isDst := isDst, abbreviation := abbr,
            wall := if isWall then .wall else .standard,
            utLocal := if isUt then .ut else .local,
            identifier := id
          }
        | _, _, _, _, _, _, _ => .error "Invalid LocalTimeType format"

      let initialLtt ← decodeLocalTimeType 0
      match args[7]! with
      | .compound "Transitions" transArray =>
        let transitions ← transArray.mapM fun t => do
          let tArgs ← decodeCompound "T" t
          if tArgs.size = 9 then do
            match tArgs[0]!, tArgs[1]! with
            | .nat time, .bool isPos =>
              let ltt ← decodeLocalTimeType 2  -- Skip time fields
              .ok { time := { val := if isPos then Int.ofNat time else -Int.ofNat time }, localTimeType := ltt }
            | _, _ => .error "Invalid Transition time format"
          else .error "Transition expects 9 args"
        .ok { initialLocalTimeType := initialLtt, transitions := transitions }
      | _ => .error "Expected Transitions compound"
    else .error "ZoneRules expects at least 8 args"

-- ZonedDateTime serialization
instance : Serializable Std.Time.ZonedDateTime where
  encode zdt := .compound "ZonedDateTime" #[
    encode zdt.timestamp,
    encode zdt.timezone
  ]
  decode sv := do
    let args ← decodeCompound "ZonedDateTime" sv
    if args.size = 2 then do
      let timestamp : Std.Time.Timestamp ← decode args[0]!
      let timezone : Std.Time.TimeZone ← decode args[1]!
      .ok (Std.Time.ZonedDateTime.ofTimestampWithZone timestamp timezone)
    else
      .error s!"ZonedDateTime expects 2 args, got {args.size}"

end LeanSerde
