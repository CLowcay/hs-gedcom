{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Gedcom (
  module Data.Gedcom.Structure,
  GDRefError (..),
  GDError (..),
  XRefTable, gdLookup,
  parseGedcomString,
  parseGedcomFile
) where

import Control.Applicative
import Data.Dynamic
import Data.Either
import Data.Gedcom.Common
import Data.Gedcom.LineParser
import Data.Gedcom.ParseMonads
import Data.Gedcom.Parser
import Data.Gedcom.Structure
import Data.Maybe
import Data.Monoid
import Data.Text.Encoding.ANSEL
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text.All as T
import Text.Megaparsec

newtype XRefTable = XRefTable (M.Map GDXRefID Dynamic) deriving Show

gdLookup :: forall a. Typeable a =>
  GDRef a -> XRefTable -> Either GDRefError a
gdLookup (GDStructure x) _ = Right x
gdLookup (GDXRef thisID) (XRefTable table) =
  case M.lookup thisID table of
    Nothing -> Left$ RefNotPresent thisID
    Just (dynamic) -> case fromDynamic dynamic of
      Nothing -> Left$
        WrongRefType (dynTypeRep dynamic) (typeRep (Proxy :: Proxy a))
      Just v -> Right v

parseGedcomString ::
  Maybe String -> B.ByteString -> Either GDError (Gedcom, XRefTable)
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
      "Invalid format (is " <> (T.show filename) <> " really a gedcom file?)"
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

parseGedcomFile :: FilePath -> IO (Either GDError (Gedcom, XRefTable))
parseGedcomFile path = parseGedcomString (Just path) <$> B.readFile path

