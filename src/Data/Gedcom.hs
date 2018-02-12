{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Data.Gedcom
Description: Parser for the GEDCOM genealogy format
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC
-}
module Data.Gedcom (
  -- * Functions
  parseGedcomString,
  parseGedcomFile,
  GDError (..),
  gdLookup,
  GDRef, XRefTable,
  GDRefError (..), GDXRefID,
  module Data.Gedcom.Structure
) where

import Control.Applicative
import Data.Dynamic
import Data.Either
import Data.Gedcom.Internal.CoreTypes
import Data.Gedcom.Internal.LineParser
import Data.Gedcom.Internal.ParseMonads
import Data.Gedcom.Internal.Parser
import Data.Gedcom.Structure
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding.ANSEL
import Data.Typeable
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.All as T
import Text.Megaparsec

-- | A table of cross references
newtype XRefTable = XRefTable (M.Map GDXRefID Dynamic) deriving Show

-- | Lookup up a reference in the cross reference table
gdLookup :: forall a. Typeable a
  => GDRef a             -- ^ The reference to look up
  -> XRefTable           -- ^ The table to look up in
  -> Either GDRefError a -- ^ The value or an error
gdLookup (GDStructure x) _ = Right x
gdLookup (GDXRef thisID) (XRefTable table) =
  case M.lookup thisID table of
    Nothing -> Left$ RefNotPresent thisID
    Just dynamic -> case fromDynamic dynamic of
      Nothing -> Left$
        WrongRefType (dynTypeRep dynamic) (typeRep (Proxy :: Proxy a))
      Just v -> Right v

-- | Parse Gedcom data from a ByteString
parseGedcomString ::
     Maybe String  -- ^ The filename from which the string was read
  -> B.ByteString  -- ^ The string to parse
  -> Either GDError (Gedcom, XRefTable) -- ^ The Gedcom data and cross reference table, or an error
parseGedcomString mfilename intext =
  let
    filename = fromMaybe "<<NO FILE>>" mfilename
    anselTree = runParser gdRoot filename . decodeANSEL$ intext
    utf8Tree = runParser gdRoot filename . T.decodeUtf8$ intext
    utf16LETree = runParser gdRoot filename . T.decodeUtf16LE$ intext
    utf16BETree = runParser gdRoot filename . T.decodeUtf16BE$ intext
    encodings = [anselTree, utf8Tree, utf16LETree, utf16BETree]
    charset = foldr (<|>) Nothing . fmap getCharset$ encodings
    trees = case charset of
      Nothing -> []
      Just (Charset "ANSEL" _) -> [anselTree]
      Just (Charset "UTF-8" _) -> [utf8Tree]
      Just (Charset "UNICODE" _) -> [utf8Tree, utf16LETree, utf16BETree]
      Just (Charset "ASCII" _) -> [utf8Tree, anselTree]
      Just (Charset _ _) -> [anselTree, utf8Tree]

  in case partitionEithers trees of
    ([], []) -> Left . LineFormatError$
      "Invalid format (is " <> T.show filename <> " really a gedcom file?)"
    (err:_, []) -> Left . LineFormatError . T.show$ err
    (_, dtrees) -> case partitionEithers.fmap doParseGedcom$ dtrees of
      ([], []) -> Left . LineFormatError$ "Unknown character encoding"
      (err:_, []) -> Left err
      (_, (gd, table):_) -> Right (gd, XRefTable table)
  where
    doParseGedcom tree = case parseGedcom tree of
      (Left err, _) -> Left err
      (Right v, table) -> Right (v, table)
    getCharset (Right (GDRoot (headTree:_))) =
      case runStructure$ parseHeader headTree of
        (Right (Right header), _) -> Just$ headerCharset header
        _ -> Nothing
    getCharset _ = Nothing

-- | Parse Gedcom data from a file
parseGedcomFile ::
  FilePath -- ^ The file to read
  -> IO (Either GDError (Gedcom, XRefTable)) -- ^ The Gedcom data and cross reference table, or an error
parseGedcomFile path = parseGedcomString (Just path) <$> B.readFile path

