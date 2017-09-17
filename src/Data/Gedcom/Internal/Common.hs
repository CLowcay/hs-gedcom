module Data.Gedcom.Internal.Common
  ((<&>), withDefault, trim, Parser, timeToPicos, timeValue,
  dateExact, month, monthFr, monthHeb, yearGreg
  )
where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Time.Clock
import qualified Data.Text.All as T
import Text.Megaparsec

type Parser = Parsec () T.Text

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}

withDefault :: Alternative f => a -> f a -> f a
withDefault def = fmap (fromMaybe def).optional
{-# INLINE withDefault #-}

trim :: T.Text -> T.Text
trim = T.dropWhile isSpace . T.dropWhileEnd isSpace

timeToPicos :: (Int, Int, Int, Double) -> DiffTime
timeToPicos (h, m, s, fs) =
  picosecondsToDiffTime$
    (fromIntegral h * hm)
    + (fromIntegral m * mm)
    + (fromIntegral s * sm)
    + (round$ fs * (fromIntegral sm))
  where
    hm = mm * 60
    mm = sm * 60
    sm = 1000000000

timeValue :: Parser (Maybe (Int, Int, Int, Double))
timeValue = optional$ (,,,)
  <$> (read <$> count' 1 2 digitChar)
  <*> (char ':' *> (read <$> count' 1 2 digitChar))
  <*> withDefault 0 (char ':' *> (read <$> count' 1 2 digitChar))
  <*> withDefault 0 (char '.' *> (read.("0." ++) <$> count' 1 3 digitChar))

dateExact :: Parser (Int, Word, Int)
dateExact = (,,)
  <$> (read <$> count' 1 2 digitChar)
  <*> (space *> month)
  <*> (space *> yearGreg)

month :: Parser Word
month =
  (string' "JAN" *> pure 1) <|>
  (string' "FEB" *> pure 2) <|>
  (string' "MAR" *> pure 3) <|>
  (string' "APR" *> pure 4) <|>
  (string' "MAY" *> pure 5) <|>
  (string' "JUN" *> pure 6) <|>
  (string' "JUL" *> pure 7) <|>
  (string' "AUG" *> pure 8) <|>
  (string' "SEP" *> pure 9) <|>
  (string' "OCT" *> pure 10) <|>
  (string' "NOV" *> pure 11) <|>
  (string' "DEC" *> pure 12)

monthFr :: Parser Word
monthFr =
  (string' "VEND" *> pure 1) <|>
  (string' "BRUM" *> pure 2) <|>
  (string' "FRIM" *> pure 3) <|>
  (string' "NIVO" *> pure 4) <|>
  (string' "PLUV" *> pure 5) <|>
  (string' "VENT" *> pure 6) <|>
  (string' "GERM" *> pure 7) <|>
  (string' "FLOR" *> pure 8) <|>
  (string' "PRAI" *> pure 9) <|>
  (string' "MESS" *> pure 10) <|>
  (string' "THER" *> pure 11) <|>
  (string' "FRUC" *> pure 12) <|>
  (string' "COMP" *> pure 13)

monthHeb :: Parser Word
monthHeb =
  (string' "TSH" *> pure 1) <|>
  (string' "CSH" *> pure 2) <|>
  (string' "KSL" *> pure 3) <|>
  (string' "TVT" *> pure 4) <|>
  (string' "SHV" *> pure 5) <|>
  (string' "ADR" *> pure 6) <|>
  (string' "ADS" *> pure 7) <|>
  (string' "NSN" *> pure 8) <|>
  (string' "IYR" *> pure 9) <|>
  (string' "SVN" *> pure 10) <|>
  (string' "TMZ" *> pure 11) <|>
  (string' "AAV" *> pure 12) <|>
  (string' "ELL" *> pure 13)

yearGreg :: Parser Int
yearGreg = do
  y <- read <$> count' 1 4 digitChar
  malt <- optional$ char '/' *> (read <$> count 2 digitChar)
  case malt of
    Just alt -> return$ if (y + 1) `mod` 100 == alt then y + 1 else y
    Nothing -> return y

