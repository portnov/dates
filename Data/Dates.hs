{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}
-- | Operations with dates
module Data.Dates
  (DateTime (..),
   Time (..),
   parseDate,
   pDate,
   getCurrentDateTime
  ) where

import Prelude.Unicode
import Data.Char (toUpper)
import Data.List
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.Parsec
import Text.Parsec.String
import Data.Generics

-- | Date / Time
data DateTime =
  DateTime {
    year ∷ Int,
    month ∷ Int,
    day ∷ Int,
    hour ∷ Int,
    minute ∷ Int,
    second ∷ Int }
  deriving (Eq,Ord,Data,Typeable)

-- | 12 months names.
months ∷ [String]
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]

-- | capitalize first letter of the string
capitalize ∷ String → String
capitalize [] = []
capitalize (x:xs) = (toUpper x):xs

-- | Show name of given month
showMonth ∷  Int → String
showMonth i = capitalize $ months !! (i-1)

instance Show DateTime where
  show (DateTime y m d h mins s) = 
    show d ⧺ " " ⧺ showMonth m ⧺ " " ⧺ show y ⧺ ", " ⧺
      show h ⧺ ":" ⧺ show mins ⧺ ":" ⧺ show s

-- | Only time, without date
data Time = 
  Time {
    tHour ∷ Int,
    tMinute ∷ Int,
    tSecond ∷ Int }
  deriving (Eq,Ord,Show,Data,Typeable)

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

times ∷ Int → Parser t → Parser [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts
                               
number ∷ Int → Int → Parser Int
number n m = do
  t ← read `fmap` (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ Parser Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ Parser Int
pMonth = number 2 12

pDay ∷ Parser Int
pDay = number 2 31

euroNumDate ∷ Parser DateTime
euroNumDate = do
  d ← pDay
  char '.'
  m ← pMonth
  char '.'
  y ← pYear
  return $ date y m d

americanDate ∷ Parser DateTime
americanDate = do
  y ← pYear
  char '/'
  m ← pMonth
  char '/'
  d ← pDay
  return $ date y m d

euroNumDate' ∷ Int → Parser DateTime
euroNumDate' year = do
  d ← pDay
  char '.'
  m ← pMonth
  return $ date year m d

americanDate' ∷ Int → Parser DateTime
americanDate' year = do
  m ← pMonth
  char '/'
  d ← pDay
  return $ date year m d

strDate ∷ Parser DateTime
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

strDate' ∷ Int → Parser DateTime
strDate' year = do
  d ← pDay
  space
  ms ← many1 letter
  case lookupMonth ms of
    Nothing → fail $ "unknown month: "++ms
    Just m  → return $ date year m d

time24 ∷ Parser Time
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

ampm ∷ Parser Int
ampm = do
  s ← many1 letter
  case map toUpper s of
    "AM" → return 0
    "PM" → return 12
    _ → fail "AM/PM expected"

time12 ∷ Parser Time
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

pAbsDate ∷ Int → Parser DateTime
pAbsDate year = do
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
      t ← choice $ map try [time12,time24]
      return $ date `addTime` t

data DateIntervalType = Day | Week | Month | Year
  deriving (Eq,Show,Read)

data DateInterval = Days ℤ
                  | Weeks ℤ
                  | Months ℤ
                  | Years ℤ
  deriving (Eq,Show)

convertTo ∷  DateTime → Day
convertTo dt = fromGregorian (fromIntegral $ year dt) (month dt) (day dt)

convertFrom ∷  Day → DateTime
convertFrom dt = 
  let (y,m,d) = toGregorian dt
  in  date (fromIntegral y) m d

modifyDate ∷  (t → Day → Day) → t → DateTime → DateTime
modifyDate fn x dt = convertFrom $ fn x $ convertTo dt

addInterval ∷  DateTime → DateInterval → DateTime
addInterval dt (Days ds) = modifyDate addDays ds dt
addInterval dt (Weeks ws) = modifyDate addDays (ws*7) dt
addInterval dt (Months ms) = modifyDate addGregorianMonthsClip ms dt
addInterval dt (Years ys) = modifyDate addGregorianYearsClip ys dt

maybePlural ∷ String → Parser String
maybePlural str = do
  r ← string str
  optional $ char 's'
  return (capitalize r)

pDateInterval ∷ Parser DateIntervalType
pDateInterval = do
  s ← choice $ map maybePlural ["day", "week", "month", "year"]
  return $ read s

pRelDate ∷ DateTime → Parser DateTime
pRelDate date = do
  offs ← (try futureDate) <|> (try passDate) <|> (try today) <|> (try tomorrow) <|> yesterday
  return $ date `addInterval` offs

futureDate ∷ Parser DateInterval
futureDate = do
  string "in "
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  case tp of
    Day →   return $ Days (read n)
    Week →  return $ Weeks (read n)
    Month → return $ Months (read n)
    Year →  return $ Years (read n)

passDate ∷ Parser DateInterval
passDate = do
  n ← many1 digit
  char ' '
  tp ← pDateInterval
  string " ago"
  case tp of
    Day →   return $ Days $ - (read n)
    Week →  return $ Weeks $ - (read n)
    Month → return $ Months $ - (read n)
    Year →  return $ Years $ - (read n)

today ∷ Parser DateInterval
today = do
  string "today"
  return $ Days 0

tomorrow ∷ Parser DateInterval
tomorrow = do
  string "tomorrow"
  return $ Days 1

yesterday ∷ Parser DateInterval
yesterday = do
  string "yesterday"
  return $ Days (-1)

pDate ∷ DateTime → Parser DateTime
pDate date =  (try $ pRelDate date) <|> (try $ pAbsDate $ year date)

-- | Parse date/time
parseDate ∷ DateTime  -- ^ Current date/time
          → String    -- ^ String to parse
          → Either ParseError DateTime
parseDate date s = runParser (pDate date) () "" s

