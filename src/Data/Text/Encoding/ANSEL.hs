{-|
Module: Data.Text.Encoding.ANSEL
Description: Decoder for the ANSEL character encoding
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC

ANSEL <https://en.wikipedia.org/wiki/ANSEL> is a character set and associated
encodings intended for bibliographic purposes.  GEDCOM files use the 8-bit
ANSEL encoding by default, so we need a way to decode it.  ANSEL has combining
diacritics, but they precede the character that they modify (Unicode has it the
other way around).  This means that the code points must be reordered when
converting to Unicode.

-}
module Data.Text.Encoding.ANSEL (
  decodeANSEL
) where

import Control.Monad.Loops (whileM)
import Control.Monad.State (State, evalState, get, put)
import Data.Array
import Data.Word
import qualified Data.ByteString as B
import qualified Data.Text.All as T

-- | Decode an ANSEL string to Unicode
decodeANSEL ::
  B.ByteString -- ^ The string to encode
  -> T.Text    -- ^ Unicode text
decodeANSEL bs = T.pack . concat .
  evalState (whileM ((not.null) <$> get) decodeANSELChar)$ B.unpack bs

decodeANSELChar :: State [Word8] String
decodeANSELChar = do
  (dias', rest) <- span isDiacritic <$> get
  let dias = filter (/= '\xFFFD').fmap (combiningTable!)$ dias'
  case rest of
    [] -> put [] >> if null dias then return "" else return "\xFFFD"
    r:rs -> put rs >> let c = encode r in
      if c == '\xFFFD' then return "\xFFFD" else return$ c:dias
  where encode r = if isAscii r then toEnum (fromIntegral r) else composedTable!r

isAscii :: Word8 -> Bool
isAscii x = x < 0x80

isDiacritic :: Word8 -> Bool
isDiacritic x = x >= 0xE0

composedTable :: Array Word8 Char
composedTable = accumArray (flip const) '\xFFFD' (0x00, 0xFF) [
  (0xA1, 'Ł'),
  (0xA2, 'Ø'),
  (0xA3, 'Đ'),
  (0xA4, 'Þ'),
  (0xA5, 'Æ'),
  (0xA6, 'Œ'),
  (0xA7, 'ʹ'),
  (0xA8, '·'),
  (0xA9, '♭'),
  (0xAA, '®'),
  (0xAB, '±'),
  (0xAC, 'Ơ'),
  (0xAD, 'Ư'),
  (0xAE, 'ʼ'),
  (0xB0, 'ʻ'),
  (0xB1, 'ł'),
  (0xB2, 'ø'),
  (0xB3, 'đ'),
  (0xB4, 'þ'),
  (0xB5, 'æ'),
  (0xB6, 'œ'),
  (0xB7, 'ʺ'),
  (0xB8, 'ı'),
  (0xB9, '£'),
  (0xBA, 'ð'),
  (0xBC, 'ơ'),
  (0xBD, 'ư'),
  (0xBE, '□'),
  (0xBF, '■'),
  (0xC0, '°'),
  (0xC1, 'ℓ'),
  (0xC2, '℗'),
  (0xC3, '©'),
  (0xC4, '♯'),
  (0xC5, '¿'),
  (0xC6, '¡'),
  (0xCD, 'e'),
  (0xCE, 'o'),
  (0xCF, 'ß')]

combiningTable :: Array Word8 Char
combiningTable = accumArray (flip const) '\xFFFD' (0x00, 0xFF) [
  (0xE0, '\x0309'),
  (0xE1, '\x0300'),
  (0xE2, '\x0301'),
  (0xE3, '\x0302'),
  (0xE4, '\x0303'),
  (0xE5, '\x0304'),
  (0xE6, '\x0306'),
  (0xE7, '\x0307'),
  (0xE8, '\x0308'),
  (0xE9, '\x030C'),
  (0xEA, '\x030A'),
  (0xEB, '\xFE20'),
  (0xEC, '\xFE21'),
  (0xED, '\x0315'),
  (0xEE, '\x030B'),
  (0xEF, '\x0310'),
  (0xF0, '\x0327'),
  (0xF1, '\x0328'),
  (0xF2, '\x0323'),
  (0xF3, '\x0324'),
  (0xF4, '\x0325'),
  (0xF5, '\x0333'),
  (0xF6, '\x0332'),
  (0xF7, '\x0326'),
  (0xF8, '\x031C'),
  (0xF9, '\x032E'),
  (0xFA, '\xFE22'),
  (0xFB, '\xFE23'),
  (0xFE, '\x0313'),
  (0xFF, '\x0338')]

