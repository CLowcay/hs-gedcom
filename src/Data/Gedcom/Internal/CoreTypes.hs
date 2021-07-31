{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.Gedcom.Common
-- Description: Common utility functions for parsing GEDCOM
-- Copyright: (c) Callum Lowcay, 2017 - 2021
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
module Data.Gedcom.Internal.CoreTypes
  ( GDRefError (..),
    GDError (..),
    GDRef (..),
    GDRoot (..),
    GDTree (..),
    GDLine (..),
    GDLineValue (..),
    GDLineItem (..),
    GDEscape (..),
    GDXRefID (..),
    GDTag (..),
    GDLevel (..),
    gdLineData,
    gdTrimLineItem,
    gdIgnoreEscapes,
    gdFilterEscapes,
  )
where

import Control.Arrow (Arrow (first))
import Data.Char (isSpace)
import Data.List (foldl', groupBy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (TypeRep)

-- | An error arising from dereferencing a 'GDRef'
data GDRefError
  = -- | The referred structure doesn't exist.
    RefNotPresent GDXRefID
  | -- | Dereferenced structure had the wrong type
    WrongRefType TypeRep TypeRep

instance Show GDRefError where
  show (RefNotPresent thisID) = "Missing referenced structure " ++ show thisID
  show (WrongRefType got expected) =
    "Referenced value has wrong type, expected "
      ++ show expected
      ++ " but saw "
      ++ show got

-- | A parse error.
data GDError
  = -- | A badly formatted GEDCOM line
    LineFormatError Text
  | -- | A reference where a reference wasn't allowed
    UnexpectedRef Text
  | -- | Missing a reference where a reference was required
    RequiredRef Text
  | -- | Two targets for the same reference
    DuplicateRef Text
  | -- | A badly formatted field
    FormatError Text
  | -- | The wrong tag
    TagError Text
  deriving (Show)

-- | A reference to another structure
data GDRef a
  = -- | Already dereferenced.
    GDStructure a
  | -- | The 'GDXRefID' to look up
    GDXRef GDXRefID
  deriving (Show, Eq)

-- | A raw GEDCOM syntax tree
data GDRoot = GDRoot [GDTree] deriving (Show, Eq)

-- | A GEDCOM subtree
data GDTree = GDTree GDLine [GDTree] deriving (Show, Eq)

-- | A GEDCOM line
data GDLine = GDLine GDLevel (Maybe GDXRefID) GDTag (Maybe GDLineValue)
  deriving (Show, Eq)

-- | The value field
data GDLineValue
  = GDLineItemV GDLineItem
  | GDXRefIDV GDXRefID
  deriving (Show, Eq)

-- | Line text
newtype GDLineItem = GDLineItem [(Maybe GDEscape, Text)] deriving (Show, Eq)

-- | An escape sequence
newtype GDEscape = GDEscape Text deriving (Show, Eq)

-- | A cross reference ID
newtype GDXRefID = GDXRefID Text deriving (Show, Eq, Ord)

-- | The tag field
newtype GDTag = GDTag Text deriving (Show, Eq, Ord)

-- | The level field
newtype GDLevel = GDLevel Int deriving (Show, Eq, Ord, Num)

-- | Extract the line text
gdLineData :: GDLineItem -> [(Maybe GDEscape, Text)]
gdLineData (GDLineItem v) = v

instance Monoid GDLineItem where
  mempty = GDLineItem []

instance Semigroup GDLineItem where
  (GDLineItem l1) <> (GDLineItem l2) =
    GDLineItem . fmap coalease . groupBy canCoalease $ l1 <> l2
    where
      coalease [] = (Nothing, "")
      coalease (l : ls) = foldl' (\(_, t1) (e, t2) -> (e, t1 <> t2)) l ls
      canCoalease (Nothing, _) (_, _) = True
      canCoalease _ _ = False

-- | Trim white space off the start an end of a GEDCOM line text.
gdTrimLineItem :: GDLineItem -> GDLineItem
gdTrimLineItem (GDLineItem []) = GDLineItem []
gdTrimLineItem (GDLineItem ((e, t) : rst)) =
  let rst' = reverse $ case reverse rst of
        [] -> []
        ((e', t') : rst'') -> (e', T.dropWhile isSpace t') : rst''
   in GDLineItem $ (e, T.dropWhile isSpace t) : rst'

-- | Ignore escape sequences
gdIgnoreEscapes :: [(Maybe GDEscape, Text)] -> Text
gdIgnoreEscapes = T.concat . fmap snd

-- | Ignore certain escape sequences
gdFilterEscapes ::
  [GDEscape] -> [(Maybe GDEscape, Text)] -> [(Maybe GDEscape, Text)]
gdFilterEscapes escapes =
  gdLineData . mconcat . fmap (GDLineItem . (: []) . first f)
  where
    f (Just e) = if e `elem` escapes then Just e else Nothing
    f Nothing = Nothing
