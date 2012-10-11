{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}
-- | Operations with dates
module Data.Dates
  (DateTime (..),
   Time (..),
   WeekDay (..),
   parseDate,
   pDate, pDateTime, pTime,
   pDateInterval,
   getCurrentDateTime,
   tryRead,
   DateIntervalType (..),
   DateInterval (..),
   dayToDateTime, dateTimeToDay,
   weekdayToInterval,
   weekdayNumber,
   intToWeekday,
   dateWeekDay,
   lastMonday, nextMonday,
   modifyDate,
   datesDifference,
   addInterval, negateInterval, minusInterval,
   addTime
  ) where

import Prelude.Unicode
import Data.Char (toUpper)
import Data.List
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.Time.LocalTime
import Text.Parsec
import Text.Parsec.String
import Data.Generics

import Data.Dates.Types
import Data.Dates.Internal

data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read,Data,Typeable)

data DateInterval = Days ℤ
                  | Weeks ℤ
                  | Months ℤ
                  | Years ℤ
  deriving (Eq,Show,Data,Typeable)

data WeekDay =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Eq, Show, Read, Ord, Enum, Bounded, Data, Typeable)

-- | Weekday as interval from Monday, so that
-- weekdayToInterval Monday == 0 and
-- weekdayToInterval Sunday == 6.
weekdayToInterval ∷ WeekDay → DateInterval
weekdayToInterval wd = Days (fromIntegral $ fromEnum wd)

-- | Number of weekday, with Monday == 1 and Sunday == 7.
weekdayNumber ∷ WeekDay → Int
weekdayNumber wd = fromEnum wd + 1

-- | Reverse for weekdayNumber
intToWeekday ∷ Int → WeekDay
intToWeekday i = toEnum (i - 1)

lastMonday ∷ DateTime → DateTime
lastMonday dt = dt `minusInterval` weekdayToInterval (dateWeekDay dt)

nextMonday ∷ DateTime → DateTime
nextMonday dt = lastMonday dt `addInterval` Weeks 1

-- | Get current date and time.
getCurrentDateTime ∷  IO DateTime
getCurrentDateTime = do
  zt ← getZonedTime
  let lt = zonedTimeToLocalTime zt
      ld = localDay lt
      ltod = localTimeOfDay lt
      (y,m,d) = toGregorian ld
      h = todHour ltod
      mins = todMin ltod
      s = round $ todSec ltod
  return $ DateTime (fromIntegral y) m d h mins s

-- | Get weekday of given date.
dateWeekDay ∷ DateTime → WeekDay
dateWeekDay dt =
  let (_,_,wd) = toWeekDate (dateTimeToDay dt)
  in  intToWeekday wd

uppercase ∷ String → String
uppercase = map toUpper

isPrefixOfI ∷  String → String → Bool
p `isPrefixOfI` s = (uppercase p) `isPrefixOf` (uppercase s)

lookupS ∷ String → [(String,a)] → Maybe a
lookupS _ [] = Nothing
lookupS k ((k',v):other) | k `isPrefixOfI` k' = Just v
                         | otherwise          = lookupS k other

monthsN ∷ [(String,Int)]
monthsN = zip months [1..]

lookupMonth ∷ String → Maybe Int
lookupMonth n = lookupS n monthsN

date ∷  Int → Int → Int → DateTime
date y m d = DateTime y m d 0 0 0

addTime ∷  DateTime → Time → DateTime
addTime dt t = dt {
                 hour = tHour t + hour dt,
                 minute = tMinute t + minute dt,
                 second = tSecond t + second dt }

euroNumDate ∷ Parsec String st DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ Parsec String st DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  d ← pDay
  return $ date y m d

euroNumDate' ∷ Int → Parsec String st DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Int → Parsec String st DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  d ← pDay
  return $ date year m d

strDate ∷ Parsec String st DateTime
strDate = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → do
      space
      y ← pYear
      notFollowedBy $ char ':'
      return $ date y m d

strDate' ∷ Int → Parsec String st DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ Parsec String st Time
time24 = do
  h ← number 2 23
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  case x of
    Nothing → return $ Time h m 0
    Just _ → do
      s ← number 2 59
      notFollowedBy letter
      return $ Time h m s

ampm ∷ Parsec String st Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ Parsec String st Time
time12 = do
  h ← number 2 12
  char ':'
  m ← number 2 59
  x ← optionMaybe $ char ':'
  s ← case x of
            Nothing → return 0
            Just _  → number 2 59
  optional space
  hd ← ampm
  return $ Time (h+hd) m s

pTime ∷ Parsec String st Time
pTime = choice $ map try [time12, time24]

pAbsDateTime ∷ Int → Parsec String st DateTime
pAbsDateTime year = do
  date ← choice $ map try $ map ($ year) $ [
                              const euroNumDate,
                              const americanDate,
                              const strDate,
                              strDate',
                              euroNumDate',
                              americanDate']
  optional $ char ','
  s ← optionMaybe space
  case s of
    Nothing → return date
    Just _ → do
      t ← pTime
      return $ date `addTime` t

pAbsDate ∷ Int → Parsec String st DateTime
pAbsDate year =
  choice $ map try $ map ($ year) $ [
                          const euroNumDate,
                          const americanDate,
                          const strDate,
                          strDate',
                          euroNumDate',
                          americanDate']

-- | Convert date from DateTime to Day
dateTimeToDay ∷  DateTime → Day
dateTimeToDay dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)

-- | Convert date from Day to DateTime
dayToDateTime ∷  Day → DateTime
dayToDateTime dt = 
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

-- | Modify DateTime with pure function on Day
modifyDate ∷  (t → Day → Day) → t → DateTime → DateTime
modifyDate fn x dt = dayToDateTime $ fn x $ dateTimeToDay dt

-- | Add date interval to DateTime
addInterval ∷  DateTime → DateInterval → DateTime
addInterval dt (Days ds) = modifyDate addDays ds dt
addInterval dt (Weeks ws) = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys) = modifyDate addGregorianYearsClip ys dt

-- | Negate DateInterval value: Days 3 → Days (-3).
negateInterval ∷ DateInterval → DateInterval
negateInterval (Days n)   = Days (negate n)
negateInterval (Weeks n)  = Weeks (negate n)
negateInterval (Months n) = Months (negate n)
negateInterval (Years n)  = Years (negate n)

-- | Subtract DateInterval from DateTime.
minusInterval ∷ DateTime → DateInterval → DateTime
minusInterval date int = date `addInterval` negateInterval int

-- | Number of days between two dates
datesDifference ∷ DateTime → DateTime → Integer
datesDifference d1 d2 =
  abs $ toModifiedJulianDay (dateTimeToDay d1) -
        toModifiedJulianDay (dateTimeToDay d2)

maybePlural ∷ String → Parsec String st String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateIntervalType ∷ Parsec String st DateIntervalType
pDateIntervalType = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  tryRead s

pDateInterval ∷ Parsec String st DateInterval
pDateInterval = do
  n ← many1 digit
  spaces
  tp ← pDateIntervalType
  case tp of
    Day →   Days   `fmap` tryRead n
    Week →  Weeks  `fmap` tryRead n
    Month → Months `fmap` tryRead n
    Year →  Years  `fmap` tryRead n

pRelDate ∷ DateTime → Parsec String st DateTime
pRelDate date = do
  offs ← try futureDate
     <|> try passDate
     <|> try today
     <|> try tomorrow
     <|> yesterday
  return $ date `addInterval` offs

lastWeekDay ∷ DateTime → Parsec String st DateTime
lastWeekDay now = do
  string "last"
  spaces
  wd ← try (string "week" >> return Monday) <|> pWeekDay
  let monday = lastMonday now
      monday' = if wd > dateWeekDay now
                  then monday `minusInterval` Weeks 1
                  else monday
  return $ monday' `addInterval` weekdayToInterval wd

nextWeekDay ∷ DateTime → Parsec String st DateTime
nextWeekDay now = do
  string "next"
  spaces
  wd ← try (string "week" >> return Monday) <|> pWeekDay
  let monday = nextMonday now
      monday' = if wd >= dateWeekDay now
                  then monday `minusInterval` Weeks 1
                  else monday
  return $ monday' `addInterval` weekdayToInterval wd

pWeekDay ∷ Parsec String st WeekDay
pWeekDay = do
  w ← many1 (oneOf "mondaytueswnhrfi")
  tryRead (capitalize w)

futureDate ∷ Parsec String st DateInterval
futureDate = do
  string "in "
  n ← many1 digit
  char ' '
  tp ← pDateIntervalType
  case tp of
    Day →   Days   `fmap` tryRead n
    Week →  Weeks  `fmap` tryRead n
    Month → Months `fmap` tryRead n
    Year →  Years  `fmap` tryRead n

passDate ∷ Parsec String st DateInterval
passDate = do
  n ← many1 digit
  char ' '
  tp ← pDateIntervalType
  string " ago"
  case tp of
    Day →   (Days   . negate) `fmap` tryRead n
    Week →  (Weeks  . negate) `fmap` tryRead n
    Month → (Months . negate) `fmap` tryRead n
    Year →  (Years  . negate) `fmap` tryRead n

today ∷ Parsec String st DateInterval
today = do
  string "today" <|> string "now"
  return $ Days 0

tomorrow ∷ Parsec String st DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday ∷ Parsec String st DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pByWeek ∷ DateTime → Parsec String st DateTime
pByWeek date =
  try (lastWeekDay date) <|> nextWeekDay date

-- | Parsec parser for DateTime.
pDateTime ∷ DateTime       -- ^ Current date / time, to use as base for relative dates
          → Parsec String st DateTime
pDateTime date =
      (try $ pRelDate date)
  <|> (try $ pByWeek date)
  <|> (try $ pAbsDateTime $ year date)

-- | Parsec parser for Date only.
pDate ∷ DateTime       -- ^ Current date / time, to use as base for relative dates
          → Parsec String st DateTime
pDate date =
      (try $ pRelDate date)
  <|> (try $ pByWeek date)
  <|> (try $ pAbsDate $ year date)

-- | Parse date/time
parseDate ∷ DateTime  -- ^ Current date / time, to use as base for relative dates
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDate date s = runParser (pDate date) () "" s

