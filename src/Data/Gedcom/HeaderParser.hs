{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Data.Gedcom.HeaderParser where

import Control.Monad
import Data.Gedcom.CommonParser
import Data.Gedcom.Internal.Common
import Data.Gedcom.LineParser
import Data.Gedcom.Tags
import Data.Time.Clock
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Perm

gdHeader :: Parser Header
gdHeader = do
  GDLine n Nothing (GDTag "HEAD") Nothing <- gdLine
  when (n /= 0)$ fail "Header must occur at level 0"

  makePermParser$
    Header <$$> gdSource (n + 1)
           <|?> (Nothing, Just <$> gdSimpleLine (n + 1) (GDTag "DEST"))
           <|?> (Nothing, Just <$> gdDateTime (n + 1))
           <||> gdPointerLine (n + 1) (GDTag "SUBM")
           <|?> (Nothing, Just <$> gdPointerLine (n + 1) (GDTag "SUBN"))
           <|?> (Nothing, (Just . T.unpack) <$>
             gdSimpleLine (n + 1) (GDTag "FILE"))
           <|?> (Nothing, Just <$> gdSimpleLine (n + 1) (GDTag "COPR"))
           <||> gdGedcomForm (n + 1)
           <||> gdSimpleLine (n + 1) (GDTag "CHAR")
           <|?> (Nothing, Just <$> gdSimpleLine (n + 1) (GDTag "LANG"))
           <|?> (Nothing, Just <$> gdPlaceForm (n + 1))
           <|?> (Nothing, Just <$> gdNote (n + 1))

gdTrailer :: Parser ()
gdTrailer = do
  GDLine n Nothing (GDTag "TRLR") Nothing <- gdLine
  when (n /= 0)$ fail "Header must occur at level 0"
  return ()

mkGedcomForm :: T.Text -> GedcomForm
mkGedcomForm "LINEAGE-LINKED" = GedcomLineageLinked
mkGedcomForm f = GedcomUnsupported f

gdGedcomCharset :: GDLevel -> Parser T.Text
gdGedcomCharset n = do
  r <- gdSimpleLine n (GDTag "CHAR")
  _ <- optional$ gdSimpleLine (n + 1) (GDTag "VERS")
  return r

gdGedcomForm :: GDLevel -> Parser GedcomFormat
gdGedcomForm n = do
  GDLine n' Nothing (GDTag "GEDC") Nothing <- gdLine
  when (n /= n')$ fail$ "expected GEDC at level "
    ++ (show n) ++ " but it was at level " ++ (show n')
  GedcomFormat
    <$> gdSimpleLine (n + 1) (GDTag "VERS")
    <*> (fmap mkGedcomForm$ gdSimpleLine (n + 1) (GDTag "FORM"))

gdPlaceForm :: GDLevel -> Parser [T.Text]
gdPlaceForm n = try$ do
  GDLine n' Nothing (GDTag "PLAC") Nothing <- gdLine
  when (n /= n')$ fail$ "expected GEDC at level "
    ++ (show n) ++ " but it was at level " ++ (show n')
  form <- gdSimpleLine (n + 1) (GDTag "FORM")
  return$ T.split (== ',') form

