{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.Gedcom.LineParser
-- Description: Low-level GEDCOM parser
-- Copyright: (c) Callum Lowcay, 2017 - 2021
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
--
-- This module parses a Text string into a GEDCOM syntax tree.
module Data.Gedcom.Internal.LineParser
  ( gdRoot,
    gdDelim,
  )
where

import Control.Monad (when)
import Data.Char (isControl)
import Data.Functor ((<&>))
import Data.Gedcom.Internal.Common (Parser)
import qualified Data.Gedcom.Internal.CoreTypes as G
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
  ( MonadParsec (try),
    count',
    eitherP,
    many,
    optional,
    satisfy,
    some,
    (<|>),
  )
import Text.Megaparsec.Char (alphaNumChar, char, digitChar)

-- | Parse any_char.
gdAnyChar :: Parser Text
gdAnyChar = fmap T.singleton gdNonAt <|> "@@"

-- | Parse non_at.
gdNonAt :: Parser Char
gdNonAt = satisfy (\c -> (not . isControl) c && c /= '@' && c /= '\x7F')

-- | Parse alphanum.
gdAlphaNum :: Parser Char
gdAlphaNum = alphaNumChar <|> char '_'

-- | Parse delim.
gdDelim :: Parser (Maybe Char)
gdDelim = optional $ char '\x20'

-- | Parse escape.
gdEscape :: Parser G.GDEscape
gdEscape =
  G.GDEscape
    <$> ("@#" *> gdEscapeText <* char '@' <* optional (char ' '))

-- | Parse escape_text.
gdEscapeText :: Parser Text
gdEscapeText = T.concat <$> many gdAnyChar

-- | Parse level.
gdLevel :: Parser G.GDLevel
gdLevel = G.GDLevel . read <$> count' 1 2 digitChar <* gdDelim

-- | Parse line_item.
gdLineItem :: Parser G.GDLineItem
gdLineItem =
  fmap G.GDLineItem . some $
    (,) <$> optional gdEscape <*> (T.concat <$> some gdAnyChar)

-- | Parse pointer.
gdPointer :: Parser G.GDXRefID
gdPointer = char '@' *> plabel <* char '@'
  where
    plabel =
      fmap (G.GDXRefID . T.pack) $
        (:) <$> gdAlphaNum <*> many gdNonAt

-- | Parse line_value
gdLineValue :: Parser G.GDLineValue
gdLineValue =
  eitherP gdPointer gdLineItem <&> \case
    Left v -> G.GDXRefIDV v
    Right v -> G.GDLineItemV v

-- | Parse optional_line_value.
gdOptionalLineValue :: Parser G.GDLineValue
gdOptionalLineValue = gdDelim *> gdLineValue

-- | Parse optional_xref_ID.
gdOptionalXRefID :: Parser (Maybe G.GDXRefID)
gdOptionalXRefID = gdXRefID <* gdDelim

-- | Parse tag.
gdTag :: Parser G.GDTag
gdTag = G.GDTag . T.toUpper . T.pack <$> many gdAlphaNum

-- | parse terminator.
gdTerminator :: Parser Text
gdTerminator = "\n" <|> "\r" <|> "\r\n" <|> "\n\r"

-- | Parse xref_ID.
gdXRefID :: Parser (Maybe G.GDXRefID)
gdXRefID = optional $ fmap (\(G.GDXRefID t) -> G.GDXRefID t) gdPointer

-- | Parse gedcom_line.
gdLine :: Parser G.GDLine
gdLine =
  G.GDLine
    <$> gdLevel
    <*> gdOptionalXRefID
    <*> gdTag
    <*> optional gdOptionalLineValue <* gdTerminator

-- | Convert local ids to global ids.
gdExpandID ::
  -- | The parent structure's ID
  G.GDXRefID ->
  -- | The local ID.
  G.GDXRefID ->
  -- | A global ID.
  G.GDXRefID
gdExpandID (G.GDXRefID pid) s@(G.GDXRefID sub) =
  case T.uncons sub of
    Nothing -> s
    Just ('!', _) -> G.GDXRefID $ pid <> sub
    _ -> s

-- | Convert local ids to global ids.
gdExpandPointer ::
  -- | The parent structure's ID.
  G.GDXRefID ->
  -- | The line to update.
  G.GDLineValue ->
  -- | An updated line where local ID's have been made global.
  G.GDLineValue
gdExpandPointer pid v = case v of
  G.GDLineItemV _ -> v
  G.GDXRefIDV p -> G.GDXRefIDV $ gdExpandID pid p

-- | Parse a line at a fixed level
gdLineLevel ::
  -- | The parent ID
  G.GDXRefID ->
  -- | The level to parse the line at
  G.GDLevel ->
  Parser G.GDLine
gdLineLevel pid n = do
  (G.GDLine n' xrid tag v) <- gdLine
  when (n' /= n) $
    fail $
      "Saw a " ++ show tag
        ++ " tag at level "
        ++ show n'
        ++ " but expected level was "
        ++ show n
  pure $ G.GDLine n' (fmap (gdExpandID pid) xrid) tag (fmap (gdExpandPointer pid) v)

-- | Parse a GEDCOM subtree
gdTree ::
  -- | The parent ID
  G.GDXRefID ->
  -- | The level where the subtree is rooted
  G.GDLevel ->
  Parser G.GDTree
gdTree pid n = try $ do
  line@(G.GDLine _ pid' _ _) <- gdLineLevel pid n
  G.GDTree line <$> many (gdTree (fromMaybe pid pid') (n + 1))

-- | Parse the raw GEDCOM syntax tree.
gdRoot :: Parser G.GDRoot
gdRoot = G.GDRoot <$> many (gdTree (G.GDXRefID "") 0)
