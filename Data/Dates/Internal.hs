{-# LANGUAGE UnicodeSyntax, DeriveDataTypeable #-}

module Data.Dates.Internal where

import Text.Parsec
import Text.Parsec.String

-- | Parser version of Prelude.read
tryRead :: Read a => String -> Parsec String st a
tryRead str =
  case reads str of
    [(res, "")] -> return res
    _ -> fail $ "Cannot read: " ++ str

-- | Apply parser N times
times ∷ Int
     → Parsec String st t
     → Parsec String st [t]
times 0 _ = return []
times n p = do
  ts ← times (n-1) p
  t ← optionMaybe p
  case t of
    Just t' → return (ts ++ [t'])
    Nothing → return ts
                               
-- | Parse natural number of N digits
-- which is not greater than M
number ∷ Int   -- ^ Number of digits
       → Int   -- ^ Maximum value
       → Parsec String st Int
number n m = do
  t ← tryRead =<< (n `times` digit)
  if t > m
    then fail "number too large"
    else return t

pYear ∷ Parsec String st Int
pYear = do
  y ← number 4 10000
  if y < 2000
    then return (y+2000)
    else return y

pMonth ∷ Parsec String st Int
pMonth = number 2 12

pDay ∷ Parsec String st Int
pDay = number 2 31

