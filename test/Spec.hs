{-# LANGUAGE OverloadedStrings #-}

import Data.Gedcom.Internal.CoreTypes
import Data.Gedcom.Internal.LineParser
import Data.Gedcom.Internal.ParseMonads
import Data.Gedcom.Internal.Parser
import Data.Monoid
import qualified Data.Text.All as T
import Test.Hspec
import Text.Megaparsec

testTagData :: T.Text
testTagData = "some test data"

testTag :: Maybe GDLine
testTag = do
  (GDRoot [GDTree t []]) <- parseMaybe gdRoot$ "0 TEST " <> testTagData <> "\n"
  return t

parseStructure :: Maybe GDTree -> StructureParser a -> Maybe a
parseStructure l p = do
  v <- l
  case runStructure (p v) of
    (Right (Right v2), _) -> return v2
    _ -> Nothing

noEscapes :: T.Text -> [(Maybe GDEscape, T.Text)]
noEscapes v = [(Nothing, v)]

main :: IO ()
main = hspec$ do
  describe "gdRoot" $ do
    it "parses a simple line with no children" $
      testTag `shouldBe` (Just$ GDLine (GDLevel 0) Nothing (GDTag "TEST")
        (Just . GDLineItemV . GDLineItem . noEscapes$ testTagData))

  describe "parseNoLinkTag" $ do
    it "matches the specified tag" $
      testStructure (parseNoLinkTag (GDTag "TEST") pure) `shouldBe`
        (Just (noEscapes testTagData, []))

  where
    testStructure = parseStructure (GDTree <$> testTag <*> pure [])

