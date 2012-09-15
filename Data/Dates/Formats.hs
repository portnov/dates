{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}
-- | This module allows to parse arbitrary date formats.
-- Date formats are specified as strings:
--
--  * "DD.MM.YYY"
--
--  * "YYYY\/MM\/DD"
--
--  * "DD\/MM\/YYYY, HH:mm:SS"
--
--  * and so on.
--
module Data.Dates.Formats
  (FormatElement (..), Format,
   pFormat, formatParser,
   parseDateFormat
  ) where

import Control.Applicative ((<$>))
import Data.Monoid
import Text.Parsec

import Data.Dates.Types
import Data.Dates.Internal (number)

-- | Date\/time format element
data FormatElement =
    YEAR   Int
  | MONTH  Int
  | DAY    Int
  | HOUR   Int
  | MINUTE Int
  | SECOND Int
  | Fixed String
  deriving (Eq, Show)

-- | Date\/time format
type Format = [FormatElement]

nchars ∷ Char → Parsec String st Int
nchars c = do
  s ← many1 $ char c
  return $ length s

-- | Parser for date\/time format.
pFormat ∷ Parsec String st Format
pFormat = many1 $ choice $ map try [pYear, pMonth, pDay,
                                    pHour, pMinute, pSecond,
                                    pFixed]
  where
    pYear   = YEAR   <$> nchars 'Y'
    pMonth  = MONTH  <$> nchars 'M'
    pDay    = DAY    <$> nchars 'D'
    pHour   = HOUR   <$> nchars 'H'
    pMinute = MINUTE <$> nchars 'm'
    pSecond = SECOND <$> nchars 'S'
    pFixed  = Fixed <$> (many1 $ noneOf "YMDHmS")

pYear ∷ Int → Parsec String st DateTime
pYear n = do
  y ← number n 10000
  if y < 2000
    then return $ mempty {year = y+2000}
    else return $ mempty {year = y}

pMonth ∷ Int → Parsec String st DateTime
pMonth n = do
  m ← number n 12
  return $ mempty {month = m}

pDay ∷ Int → Parsec String st DateTime
pDay n = do
  d ← number n 31
  return $ mempty {day = d}

pHour ∷ Int → Parsec String st DateTime
pHour n = do
  h ← number n 23
  return $ mempty {hour = h}

pMinute ∷ Int → Parsec String st DateTime
pMinute n = do
  m ← number n 59
  return $ mempty {minute = m}

pSecond ∷ Int → Parsec String st DateTime
pSecond n = do
  s ← number n 59
  return $ mempty {second = s}

-- | Make Parser for specified date format.
formatParser ∷ Format → Parsec String st DateTime
formatParser format = mconcat <$> mapM parser format
  where
    parser (YEAR   n) = pYear n
    parser (MONTH  n) = pMonth n
    parser (DAY    n) = pDay n
    parser (HOUR   n) = pHour n
    parser (MINUTE n) = pMinute n
    parser (SECOND n) = pSecond n
    parser (Fixed s) = string s >> return mempty

-- | Parse date\/time in specified format.
parseDateFormat :: String  -- ^ Format string, i.e. "DD.MM.YY"
                -> String  -- ^ String to parse
                -> Either ParseError DateTime
parseDateFormat formatStr str = do
  format <- runParser pFormat () "(date format string)" formatStr
  runParser (formatParser format) () "(date)" str

