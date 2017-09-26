{-# LANGUAGE ScopedTypeVariables #-}
module Data.Gedcom (
  module Data.Gedcom.Structure,
  GDRefError (..),
  GDError (..),
  XRefTable, gdLookup,
  Data.Gedcom.parseGedcom,
) where

import Data.Dynamic
import Data.Gedcom.Common
import Data.Gedcom.LineParser
import Data.Gedcom.Parser
import Data.Gedcom.Structure
import Data.Maybe
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

parseGedcom :: Maybe String -> T.Text -> Either GDError (Gedcom, XRefTable)
parseGedcom mfilename intext =
  case runParser gdRoot (fromMaybe "<<UNKNOWN FILE>>" mfilename) intext of
    Left err -> Left$ LineFormatError (T.show err)
    Right tree -> case Data.Gedcom.Parser.parseGedcom tree of
      (Left err, _) -> Left err
      (Right v, table) -> Right (v, XRefTable table)

