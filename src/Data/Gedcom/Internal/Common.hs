{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.Gedcom.Internal.Common
-- Description: Common utility functions for parsing
-- Copyright: (c) Callum Lowcay, 2017 - 2021
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
module Data.Gedcom.Internal.Common
  ( withDefault,
    trim,
    Parser,
    timeToPicos,
    timeValue,
    dateExact,
    month,
    monthFr,
    monthHeb,
    yearGreg,
    showt,
  )
where

import Control.Applicative (Alternative ((<|>)), optional)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import Data.Void (Void)
import Text.Megaparsec (Parsec, count, count')
import Text.Megaparsec.Char (char, digitChar, space, string')

-- | Parsers from 'Text' using the default error component.
type Parser = Parsec Void Text

-- | Replace failure with a default value
withDefault :: Alternative f => a -> f a -> f a
withDefault def = fmap (fromMaybe def) . optional

-- | Show a value as 'Text'
showt :: Show a => a -> Text
showt = T.pack . show

-- | Trim leading and trailing whitespace off a string
trim :: Text -> Text
trim = T.dropWhile isSpace . T.dropWhileEnd isSpace

-- | Convert h:m:s.fs time to 'DiffTime'
timeToPicos :: (Int, Int, Int, Double) -> DiffTime
timeToPicos (h, m, s, fs) =
  picosecondsToDiffTime $
    (fromIntegral h * hm)
      + (fromIntegral m * mm)
      + (fromIntegral s * sm)
      + round (fs * fromIntegral sm)
  where
    hm = mm * 60
    mm = sm * 60
    sm = 1000000000

-- | Parse a GEDCOM exact time value into (h, m, s, fs) format
timeValue :: Parser (Maybe (Int, Int, Int, Double))
timeValue =
  optional $
    (,,,)
      <$> (read <$> count' 1 2 digitChar)
      <*> (char ':' *> (read <$> count' 1 2 digitChar))
      <*> withDefault 0 (char ':' *> (read <$> count' 1 2 digitChar))
      <*> withDefault 0 (char '.' *> (read . ("0." ++) <$> count' 1 3 digitChar))

-- | Parse a GEDCOM exact date into (day, month, year).  Months are number from
-- 1 to 12.
dateExact :: Parser (Int, Word, Int)
dateExact =
  (,,)
    <$> (read <$> count' 1 2 digitChar)
    <*> (space *> month)
    <*> (space *> yearGreg)

-- | Parse a Gregorian/Julian month
month :: Parser Word
month =
  (string' "JAN" $> 1)
    <|> (string' "FEB" $> 2)
    <|> (string' "MAR" $> 3)
    <|> (string' "APR" $> 4)
    <|> (string' "MAY" $> 5)
    <|> (string' "JUN" $> 6)
    <|> (string' "JUL" $> 7)
    <|> (string' "AUG" $> 8)
    <|> (string' "SEP" $> 9)
    <|> (string' "OCT" $> 10)
    <|> (string' "NOV" $> 11)
    <|> (string' "DEC" $> 12)

-- | Parse a French calendar month
monthFr :: Parser Word
monthFr =
  (string' "VEND" $> 1)
    <|> (string' "BRUM" $> 2)
    <|> (string' "FRIM" $> 3)
    <|> (string' "NIVO" $> 4)
    <|> (string' "PLUV" $> 5)
    <|> (string' "VENT" $> 6)
    <|> (string' "GERM" $> 7)
    <|> (string' "FLOR" $> 8)
    <|> (string' "PRAI" $> 9)
    <|> (string' "MESS" $> 10)
    <|> (string' "THER" $> 11)
    <|> (string' "FRUC" $> 12)
    <|> (string' "COMP" $> 13)

-- | Parse a Hebrew calendar month
monthHeb :: Parser Word
monthHeb =
  (string' "TSH" $> 1)
    <|> (string' "CSH" $> 2)
    <|> (string' "KSL" $> 3)
    <|> (string' "TVT" $> 4)
    <|> (string' "SHV" $> 5)
    <|> (string' "ADR" $> 6)
    <|> (string' "ADS" $> 7)
    <|> (string' "NSN" $> 8)
    <|> (string' "IYR" $> 9)
    <|> (string' "SVN" $> 10)
    <|> (string' "TMZ" $> 11)
    <|> (string' "AAV" $> 12)
    <|> (string' "ELL" $> 13)

-- | Parse a Gregorian year.  GEDCOM allows one to specify two versions of the
-- same year for cases where the historical year started in March instead of
-- January.  This function attempts to return the modern year number (assuming
-- the year starts in January.
yearGreg :: Parser Int
yearGreg = do
  y <- read <$> count' 1 4 digitChar
  malt <- optional $ char '/' *> (read <$> count 2 digitChar)
  case malt of
    Just alt -> pure $ if (y + 1) `mod` 100 == alt then y + 1 else y
    Nothing -> pure y
