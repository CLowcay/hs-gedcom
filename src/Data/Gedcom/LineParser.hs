{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-|
Module: Data.Gedcom.LineParser
Description: Low-level GEDCOM parser
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC

This module parses a Text string into a GEDCOM syntax tree.

-}
module Data.Gedcom.LineParser (
  gdRoot, gdDelim
) where

import Control.Monad
import Data.Char
import Data.Gedcom.Common
import Data.Gedcom.Internal.Common
import Data.Maybe
import Data.Monoid
import qualified Data.Text.All as T
import Text.Megaparsec

-- | Parse any_char.
gdAnyChar :: Parser String
gdAnyChar = (fmap (:[]) gdNonAt) <|> string "@@"

-- | Parse non_at.
gdNonAt :: Parser Char
gdNonAt = satisfy (\c -> (not.isControl) c && c /= '@' && c /= '\x7F')

-- | Parse alphanum.
gdAlphaNum :: Parser Char
gdAlphaNum = alphaNumChar <|> char '_'

-- | Parse delim.
gdDelim :: Parser (Maybe Char)
gdDelim = optional$ char '\x20'

-- | Parse escape.
gdEscape :: Parser GDEscape
gdEscape = GDEscape . T.pack <$>
  (string "@#" *> gdEscapeText <* char '@' <* (optional$ char ' '))

-- | Parse escape_text.
gdEscapeText :: Parser String
gdEscapeText = concat <$> many gdAnyChar

-- | Parse level.
gdLevel :: Parser GDLevel
gdLevel = GDLevel . read <$> count' 1 2 digitChar <* gdDelim

-- | Parse line_item.
gdLineItem :: Parser GDLineItem
gdLineItem = fmap GDLineItem . some$
  (,) <$> optional gdEscape <*> (T.pack . concat <$> some gdAnyChar)

-- | Parse pointer.
gdPointer :: Parser GDXRefID
gdPointer = char '@' *> plabel <* char '@'
  where plabel = fmap (GDXRefID . T.pack)$
                  (:) <$> gdAlphaNum <*> (many gdNonAt)

-- | Parse line_value
gdLineValue :: Parser GDLineValue
gdLineValue = eitherP gdPointer gdLineItem <&> \x -> case x of
  Left v -> GDXRefIDV v
  Right v -> GDLineItemV v

-- | Parse optional_line_value.
gdOptionalLineValue :: Parser GDLineValue
gdOptionalLineValue = gdDelim *> gdLineValue

-- | Parse optional_xref_ID.
gdOptionalXRefID :: Parser (Maybe GDXRefID)
gdOptionalXRefID = gdXRefID <* gdDelim

-- | Parse tag.
gdTag :: Parser GDTag
gdTag = GDTag . T.toUpper . T.pack <$> many gdAlphaNum

-- | parse terminator.
gdTerminator :: Parser String
gdTerminator = string "\n" <|> string "\r" <|> string "\r\n" <|> string "\n\r"

-- | Parse xref_ID.
gdXRefID :: Parser (Maybe GDXRefID)
gdXRefID = optional $ fmap (\(GDXRefID t) -> GDXRefID t) gdPointer

-- | Parse gedcom_line.
gdLine :: Parser GDLine
gdLine = GDLine <$>
  gdLevel <*>
  gdOptionalXRefID <*>
  gdTag <*>
  (optional gdOptionalLineValue) <* gdTerminator

-- | Convert local ids to global ids.
gdExpandID ::
     GDXRefID -- ^ The parent structure's ID
  -> GDXRefID -- ^ The local ID.
  -> GDXRefID -- ^ A global ID.
gdExpandID (GDXRefID pid) s@(GDXRefID sub) =
  case T.uncons sub of
    Nothing -> s
    Just ('!', _) -> GDXRefID$ pid <> sub
    _ -> s

-- | Convert local ids to global ids.
gdExpandPointer ::
     GDXRefID     -- ^ The parent structure's ID.
  -> GDLineValue  -- ^ The line to update.
  -> GDLineValue  -- ^ An updated line where local ID's have been made global.
gdExpandPointer pid v = case v of
  GDLineItemV _ -> v
  GDXRefIDV p -> GDXRefIDV$ gdExpandID pid p

-- | Parse a line at a fixed level
gdLineLevel ::
     GDXRefID -- ^ The parent ID 
  -> GDLevel  -- ^ The level to parse the line at
  -> Parser GDLine
gdLineLevel pid n = do
  (GDLine n' xrid tag v) <- gdLine
  when (n' /= n)$ fail$ "Saw a " ++ (show tag) ++
    " tag at level " ++ (show n') ++
    " but expected level was " ++ (show n)
  return$ GDLine n' (fmap (gdExpandID pid) xrid) tag (fmap (gdExpandPointer pid) v)

-- | Parse a GEDCOM subtree
gdTree ::
     GDXRefID -- ^ The parent ID
  -> GDLevel  -- ^ The level where the subtree is rooted
  -> Parser GDTree
gdTree pid n = try$ do
  line@(GDLine _ pid' _ _) <- gdLineLevel pid n
  GDTree line <$> many (gdTree (fromMaybe pid pid') (n + 1))

-- | Parse the raw GEDCOM syntax tree.
gdRoot :: Parser GDRoot
gdRoot = GDRoot <$> (many$ gdTree (GDXRefID "") 0)

