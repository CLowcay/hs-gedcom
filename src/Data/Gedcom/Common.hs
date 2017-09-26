{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Gedcom.Common (
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

data GDRefError =
  RefNotPresent GDXRefID | WrongRefType TypeRep TypeRep

instance Show GDRefError where
  show (RefNotPresent thisID) = "Missing referenced structure " ++ (show thisID)
  show (WrongRefType got expected) = "Referenced value has wrong type, expected "
    ++ (show expected) ++ " but saw " ++ (show got)

data GDError =
    LineFormatError T.Text
  | UnexpectedRef T.Text
  | RequiredRef T.Text
  | DuplicateRef T.Text
  | FormatError T.Text
  | TagError T.Text deriving Show

data GDRef a = GDStructure a | GDXRef GDXRefID deriving Show

data GDRoot = GDRoot [GDTree] deriving Show
data GDTree = GDTree GDLine [GDTree] deriving Show
data GDLine = GDLine GDLevel (Maybe GDXRefID) GDTag (Maybe GDLineValue)
  deriving Show
data GDLineValue =
  GDLineItemV GDLineItem | GDXRefIDV GDXRefID deriving (Show, Eq)
newtype GDLineItem = GDLineItem [(Maybe GDEscape, T.Text)] deriving (Show, Eq)
newtype GDEscape = GDEscape T.Text deriving (Show, Eq)
newtype GDXRefID = GDXRefID T.Text deriving (Show, Eq, Ord)
newtype GDTag = GDTag T.Text deriving (Show, Eq, Ord)
newtype GDLevel = GDLevel Int deriving (Show, Eq, Ord, Num)

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

gdTrimLineItem :: GDLineItem -> GDLineItem
gdTrimLineItem (GDLineItem []) = GDLineItem []
gdTrimLineItem (GDLineItem ((e, t):rst)) =
  let
    rst' = reverse$ case reverse rst of
      [] -> []
      ((e', t'):rst'') -> (e', T.dropWhile isSpace t'):rst''
  in GDLineItem$ (e, T.dropWhile isSpace t):rst'

gdIgnoreEscapes :: [(Maybe GDEscape, T.Text)] -> T.Text
gdIgnoreEscapes  = T.concat . fmap snd

gdFilterEscapes ::
  [GDEscape] -> [(Maybe GDEscape, T.Text)] -> [(Maybe GDEscape, T.Text)]
gdFilterEscapes escapes =
  gdLineData . mconcat . fmap GDLineItem . fmap (:[]) . fmap (first f)
  where
    f (Just e) = if e `elem` escapes then Just e else Nothing
    f Nothing = Nothing

