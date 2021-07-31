{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Data.Gedcom
-- Description: Parser for the GEDCOM genealogy format
-- Copyright: (c) Callum Lowcay, 2017
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
module Data.Gedcom
  ( -- * Functions
    parseGedcomString,
    parseGedcomFile,
    G.GDError (..),
    gdLookup,
    G.GDRef,
    XRefTable,
    G.GDRefError (..),
    G.GDXRefID,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Dynamic (Dynamic, Typeable, dynTypeRep, fromDynamic)
import Data.Either (partitionEithers)
import Data.Gedcom.Internal.Common (showt)
import qualified Data.Gedcom.Internal.CoreTypes as G
import Data.Gedcom.Internal.LineParser (gdRoot)
import Data.Gedcom.Internal.ParseMonads (runStructure)
import Data.Gedcom.Internal.Parser (parseGedcom, parseHeader)
import qualified Data.Gedcom.Structure as G
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.ANSEL (decodeANSEL)
import Data.Typeable (Proxy (..), typeRep)
import Text.Megaparsec (runParser)

-- | A table of cross references
newtype XRefTable = XRefTable (Map G.GDXRefID Dynamic) deriving (Show)

-- | Lookup up a reference in the cross reference table
gdLookup ::
  forall a.
  Typeable a =>
  -- | The reference to look up
  G.GDRef a ->
  -- | The table to look up in
  XRefTable ->
  -- | The value or an error
  Either G.GDRefError a
gdLookup (G.GDStructure x) _ = Right x
gdLookup (G.GDXRef thisID) (XRefTable table) =
  case M.lookup thisID table of
    Nothing -> Left $ G.RefNotPresent thisID
    Just dynamic -> case fromDynamic dynamic of
      Nothing ->
        Left $
          G.WrongRefType (dynTypeRep dynamic) (typeRep (Proxy :: Proxy a))
      Just v -> Right v

-- | Parse Gedcom data from a 'ByteString'
parseGedcomString ::
  -- | The filename from which the string was read
  Maybe String ->
  -- | The string to parse
  ByteString ->
  -- | The Gedcom data and cross reference table, or an error
  Either G.GDError (G.Gedcom, XRefTable)
parseGedcomString mfilename intext =
  let filename = fromMaybe "<<NO FILE>>" mfilename
      anselTree = runParser gdRoot filename . decodeANSEL $ intext
      utf8Tree = runParser gdRoot filename . T.decodeUtf8 $ intext
      utf16LETree = runParser gdRoot filename . T.decodeUtf16LE $ intext
      utf16BETree = runParser gdRoot filename . T.decodeUtf16BE $ intext
      encodings = [anselTree, utf8Tree, utf16LETree, utf16BETree]
      charset = foldr (<|>) Nothing . fmap getCharset $ encodings
      trees = case charset of
        Nothing -> []
        Just (G.Charset "ANSEL" _) -> [anselTree]
        Just (G.Charset "UTF-8" _) -> [utf8Tree]
        Just (G.Charset "UNICODE" _) -> [utf8Tree, utf16LETree, utf16BETree]
        Just (G.Charset "ASCII" _) -> [utf8Tree, anselTree]
        Just (G.Charset _ _) -> [anselTree, utf8Tree]
   in case partitionEithers trees of
        ([], []) ->
          Left . G.LineFormatError $
            "Invalid format (is " <> showt filename <> " really a gedcom file?)"
        (err : _, []) -> Left . G.LineFormatError . showt $ err
        (_, dtrees) -> case partitionEithers . fmap doParseGedcom $ dtrees of
          ([], []) -> Left . G.LineFormatError $ "Unknown character encoding"
          (err : _, []) -> Left err
          (_, (gd, table) : _) -> Right (gd, XRefTable table)
  where
    doParseGedcom tree = case parseGedcom tree of
      (Left err, _) -> Left err
      (Right v, table) -> Right (v, table)
    getCharset (Right (G.GDRoot (headTree : _))) =
      case runStructure $ parseHeader headTree of
        (Right (Right header), _) -> Just $ G.headerCharset header
        _ -> Nothing
    getCharset _ = Nothing

-- | Parse Gedcom data from a file
parseGedcomFile ::
  -- | The file to read
  FilePath ->
  -- | The Gedcom data and cross reference table, or an error
  IO (Either G.GDError (G.Gedcom, XRefTable))
parseGedcomFile path = parseGedcomString (Just path) <$> B.readFile path
