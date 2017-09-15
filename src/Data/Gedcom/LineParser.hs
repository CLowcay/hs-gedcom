{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Gedcom.LineParser where

import Control.Monad
import Data.Char
import Data.Gedcom.Internal.Common
import Data.Maybe
import Data.Monoid
import qualified Data.Text.All as T
import Text.Megaparsec

data GDRoot = GDRoot [GDTree] deriving Show
data GDTree = GDTree GDLine [GDTree] deriving Show
data GDLine = GDLine GDLevel (Maybe GDXRefID) GDTag (Maybe GDLineValue)
  deriving Show
data GDLineValue =
  GDLineItemV GDLineItem | GDXRefIDV GDXRefID deriving (Show, Eq)
newtype GDLineItem = GDLineItem T.Text deriving (Show, Eq, Ord)
newtype GDXRefID = GDXRefID T.Text deriving (Show, Eq, Ord)
newtype GDTag = GDTag T.Text deriving (Show, Eq, Ord)
newtype GDLevel = GDLevel Int deriving (Show, Eq, Ord, Num)

gdAnyChar :: Parser String
gdAnyChar = (fmap (:[]) gdNonAt) <|> string "@@"

gdNonAt :: Parser Char
gdNonAt = satisfy (\c -> (not.isControl) c && c /= '@' && c /= '\x7F')

gdAlpha :: Parser Char
gdAlpha = letterChar <|> char '_'

gdAlphaNum :: Parser Char
gdAlphaNum = alphaNumChar <|> char '_'

gdOtherChar :: Parser Char
gdOtherChar =
  satisfy (\c -> (not.isAscii) c && (not.isControl) c)
  <|> oneOf ("!\"$%&'()*+,-./;<=>?[\\]^`{|}~" :: String)

gdDelim :: Parser (Maybe Char)
gdDelim = optional$ char '\x20'

gdEscape :: Parser String
gdEscape = string "@#" *> gdEscapeText <* char '@' <* (optional$ char ' ')

gdEscapeText :: Parser String
gdEscapeText = concat <$> many gdAnyChar

gdLevel :: Parser GDLevel
gdLevel = GDLevel . read <$> count' 1 2 digitChar

gdLineItem :: Parser GDLineItem
gdLineItem = fmap (GDLineItem . T.pack . concat) . some$
  gdEscape <|> (concat <$> some gdAnyChar)

gdPointer :: Parser GDXRefID
gdPointer = char '@' *> plabel <* char '@'
  where plabel = fmap (GDXRefID . T.pack)$
                  (:) <$> gdAlphaNum <*> (many gdNonAt)

gdLineValue :: Parser GDLineValue
gdLineValue = eitherP gdPointer gdLineItem <&> \x -> case x of
  Left v -> GDXRefIDV v
  Right v -> GDLineItemV v

gdOptionalLineValue :: Parser GDLineValue
gdOptionalLineValue = gdDelim *> gdLineValue

gdOptionalXRefID :: Parser (Maybe GDXRefID)
gdOptionalXRefID = gdXRefID <* gdDelim

gdTag :: Parser GDTag
gdTag = GDTag . T.pack <$> many gdAlphaNum

gdTerminator :: Parser String
gdTerminator = string "\n" <|> string "\r" <|> string "\r\n" <|> string "\n\r"

gdXRefID :: Parser (Maybe GDXRefID)
gdXRefID = optional $ fmap (\(GDXRefID t) -> GDXRefID t) gdPointer

gdLine :: Parser GDLine
gdLine = GDLine <$>
  gdLevel <*>
  gdOptionalXRefID <*>
  gdTag <*>
  (optional gdOptionalLineValue) <* gdTerminator

gdExpandID :: GDXRefID -> GDXRefID -> GDXRefID
gdExpandID (GDXRefID pid) s@(GDXRefID sub) =
  case T.uncons sub of
    Nothing -> s
    Just ('!', _) -> GDXRefID$ pid <> sub
    _ -> s

gdExpandPointer :: GDXRefID -> GDLineValue -> GDLineValue
gdExpandPointer pid v = case v of
  GDLineItemV _ -> v
  GDXRefIDV p -> GDXRefIDV$ gdExpandID pid p

gdLineLevel :: GDXRefID -> GDLevel -> Parser GDLine
gdLineLevel pid n = do
  (GDLine n' xrid tag v) <- try gdLine
  when (n' /= n)$ fail$ "Saw a " ++ (show tag) ++
    " tag at level " ++ (show n') ++
    " but expected level was " ++ (show n)
  return$ GDLine n' (fmap (gdExpandID pid) xrid) tag (fmap (gdExpandPointer pid) v)

gdTree :: GDXRefID -> GDLevel -> Parser GDTree
gdTree pid n = do
  line@(GDLine _ pid' _ _) <- gdLineLevel pid n
  GDTree line <$> many (gdTree (fromMaybe pid pid') (n + 1))

gdRoot :: Parser GDRoot
gdRoot = GDRoot <$> (many$ gdTree (GDXRefID "") 0)
  
