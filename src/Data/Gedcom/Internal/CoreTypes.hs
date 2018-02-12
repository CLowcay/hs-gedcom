{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Data.Gedcom.Common
Description: Common utility functions for parsing GEDCOM
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC
-}
module Data.Gedcom.Internal.CoreTypes (
  GDRefError (..),
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
  gdFilterEscapes
) where

import Control.Arrow
import Data.Char
import Data.List
import Data.Monoid
import Data.Typeable
import qualified Data.Text.All as T

-- | An error arising from dereferencing a 'GDRef'
data GDRefError =
    RefNotPresent GDXRefID         -- ^ The referred structure doesn't exist.
  | WrongRefType TypeRep TypeRep   -- ^ Dereferenced structure had the wrong type

instance Show GDRefError where
  show (RefNotPresent thisID) = "Missing referenced structure " ++ show thisID
  show (WrongRefType got expected) = "Referenced value has wrong type, expected "
    ++ show expected ++ " but saw " ++ show got

-- | A parse error.
data GDError =
    LineFormatError T.Text -- ^ A badly formatted GEDCOM line
  | UnexpectedRef T.Text   -- ^ A reference where a reference wasn't allowed
  | RequiredRef T.Text     -- ^ Missing a reference where a reference was required
  | DuplicateRef T.Text    -- ^ Two targets for the same reference
  | FormatError T.Text     -- ^ A badly formatted field
  | TagError T.Text        -- ^ The wrong tag
  deriving Show

-- | A reference to another structure
data GDRef a =
    GDStructure a          -- ^ Already dereferenced.
  | GDXRef GDXRefID        -- ^ The 'GDXRefID' to look up
  deriving (Show, Eq)

-- | A raw GEDCOM syntax tree
data GDRoot = GDRoot [GDTree] deriving (Show, Eq)

-- | A GEDCOM subtree
data GDTree = GDTree GDLine [GDTree] deriving (Show, Eq)

-- | A GEDCOM line
data GDLine = GDLine GDLevel (Maybe GDXRefID) GDTag (Maybe GDLineValue)
  deriving (Show, Eq)

-- | The value field
data GDLineValue =
  GDLineItemV GDLineItem | GDXRefIDV GDXRefID deriving (Show, Eq)

-- | Line text
newtype GDLineItem = GDLineItem [(Maybe GDEscape, T.Text)] deriving (Show, Eq)

-- | An escape sequence
newtype GDEscape = GDEscape T.Text deriving (Show, Eq)

-- | A cross reference ID
newtype GDXRefID = GDXRefID T.Text deriving (Show, Eq, Ord)

-- | The tag field
newtype GDTag = GDTag T.Text deriving (Show, Eq, Ord)

-- | The level field
newtype GDLevel = GDLevel Int deriving (Show, Eq, Ord, Num)

-- | Extract the line text
gdLineData :: GDLineItem -> [(Maybe GDEscape, T.Text)]
gdLineData (GDLineItem v) = v

instance Monoid GDLineItem where
  mempty = GDLineItem []
  mappend (GDLineItem l1) (GDLineItem l2) =
    GDLineItem . fmap coalease . groupBy canCoalease$ l1 <> l2
    where
      coalease [] = (Nothing, "") 
      coalease (l:ls) = foldl' (\(_, t1) (e, t2) -> (e, t1 <> t2)) l ls
      canCoalease (Nothing, _) (_, _) = True
      canCoalease _ _ = False

-- | Trim white space off the start an end of a GEDCOM line text.
gdTrimLineItem :: GDLineItem -> GDLineItem
gdTrimLineItem (GDLineItem []) = GDLineItem []
gdTrimLineItem (GDLineItem ((e, t):rst)) =
  let
    rst' = reverse$ case reverse rst of
      [] -> []
      ((e', t'):rst'') -> (e', T.dropWhile isSpace t'):rst''
  in GDLineItem$ (e, T.dropWhile isSpace t):rst'

-- | Ignore escape sequences
gdIgnoreEscapes :: [(Maybe GDEscape, T.Text)] -> T.Text
gdIgnoreEscapes  = T.concat . fmap snd

-- | Ignore certain escape sequences
gdFilterEscapes ::
  [GDEscape] -> [(Maybe GDEscape, T.Text)] -> [(Maybe GDEscape, T.Text)]
gdFilterEscapes escapes =
  gdLineData . mconcat . fmap (GDLineItem . (:[]) . first f)
  where
    f (Just e) = if e `elem` escapes then Just e else Nothing
    f Nothing = Nothing

