{-# LANGUAGE OverloadedStrings #-}

import Data.Gedcom.Internal.Common (showt)
import qualified Data.Gedcom.Internal.CoreTypes as G
import Data.Gedcom.Internal.LineParser (gdRoot)
import Data.Gedcom.Internal.ParseMonads (StructureParser, runStructure)
import Data.Gedcom.Internal.Parser (parseBoolTag, parseLinkTag, parseListTag, parseNoLinkTag, parseTag, parseTextTag, parseWordTag)
import Data.Text (Text)
import Data.Void (Void)
import Test.Hspec (describe, hspec, it, shouldBe)
import Text.Megaparsec (parseMaybe)

testTagData :: Text
testTagData = "some test data"

testTag :: Text -> Maybe G.GDLine
testTag d = do
  (G.GDRoot [G.GDTree t []]) <- parseMaybe gdRoot $ "0 TEST " <> d <> "\n"
  return t

emptyTag :: Maybe G.GDLine
emptyTag = do
  (G.GDRoot [G.GDTree t []]) <- parseMaybe gdRoot "0 TEST\n"
  return t

ref1 :: G.GDXRefID
ref1 = G.GDXRefID "1"

linkTag :: Maybe G.GDLine
linkTag = do
  (G.GDRoot [G.GDTree t []]) <- parseMaybe gdRoot "0 TEST @1@\n"
  return t

linkLinkTag :: Maybe G.GDLine
linkLinkTag = do
  (G.GDRoot [G.GDTree t []]) <- parseMaybe gdRoot "0 @2@ TEST @1@\n"
  return t

crdTag :: Maybe G.GDLine
crdTag = do
  (G.GDRoot [G.GDTree t []]) <-
    parseMaybe gdRoot $ "0 @1@ TEST " <> testTagData <> "\n"
  return t

data ParseReturn a
  = Error Text
  | NoMatch
  | Match a
  deriving (Show, Eq)

isError :: ParseReturn a -> Bool
isError (Error _) = True
isError _ = False

parseStructure :: Maybe G.GDTree -> StructureParser a -> ParseReturn a
parseStructure Nothing _ = Error "Line parse error"
parseStructure (Just v) p = case runStructure (p v) of
  (Right (Left _), _) -> NoMatch
  (Right (Right m), _) -> Match m
  (Left err, _) -> Error $ showt err

noEscapes :: Text -> [(Maybe G.GDEscape, Text)]
noEscapes v = [(Nothing, v)]

main :: IO ()
main = hspec $ do
  describe "gdRoot" $ do
    it "parses a simple line with no children" $
      testTag testTagData
        `shouldBe` Just
          ( G.GDLine
              (G.GDLevel 0)
              Nothing
              (G.GDTag "TEST")
              (Just . G.GDLineItemV . G.GDLineItem . noEscapes $ testTagData)
          )
    it "parses a simple cross referenced line" $
      crdTag
        `shouldBe` Just
          ( G.GDLine
              (G.GDLevel 0)
              (Just ref1)
              (G.GDTag "TEST")
              (Just . G.GDLineItemV . G.GDLineItem . noEscapes $ testTagData)
          )
    it "parses a simple line cross referencing line" $
      linkTag
        `shouldBe` Just
          ( G.GDLine
              (G.GDLevel 0)
              Nothing
              (G.GDTag "TEST")
              (Just . G.GDXRefIDV $ ref1)
          )
    it "parses an empty tag with no children" $
      emptyTag
        `shouldBe` Just (G.GDLine (G.GDLevel 0) Nothing (G.GDTag "TEST") Nothing)

  describe "parseNoLinkTag" $ do
    it "matches the specified tag" $
      testLine (testTag testTagData) (parseNoLinkTag (G.GDTag "TEST") pure)
        `shouldBe` Match (noEscapes testTagData, [])
    it "matches the specified tag (with a cross reference)" $
      testLine crdTag (parseNoLinkTag (G.GDTag "TEST") pure)
        `shouldBe` Match (noEscapes testTagData, [])
    it "doesn't match other tags" $
      testLine (testTag testTagData) (parseNoLinkTag (G.GDTag "OTHER") pure)
        `shouldBe` NoMatch
    it "doesn't parse links" $
      isError $ testLine linkTag (parseNoLinkTag (G.GDTag "TEST") pure)

  describe "parseTag" $ do
    it "matches the specified tag" $
      testLine (testTag testTagData) (parseTag (G.GDTag "TEST") pure)
        `shouldBe` (Match . G.GDStructure $ (noEscapes testTagData, []))
    it "matches the specified tag (with a cross reference)" $
      testLine crdTag (parseTag (G.GDTag "TEST") pure)
        `shouldBe` (Match . G.GDStructure $ (noEscapes testTagData, []))
    it "doesn't match other tags" $
      testLine (testTag testTagData) (parseTag (G.GDTag "OTHER") pure)
        `shouldBe` NoMatch
    it "parses links" $
      testLine linkTag (parseTag (G.GDTag "TEST") pure)
        `shouldBe` Match (G.GDXRef ref1)
    it "doesn't parse links to links" $
      isError $ testLine linkLinkTag (parseTag (G.GDTag "TEST") pure)

  describe "parseLinkTag" $ do
    it "parses link tags" $
      testLine linkTag (parseLinkTag (G.GDTag "TEST"))
        `shouldBe` Match (G.GDXRef ref1 :: G.GDRef Void)
    it "doesn't parse other tags" $
      isError $
        testLine
          (testTag testTagData)
          (parseLinkTag (G.GDTag "TEST") :: StructureParser (G.GDRef Void))

  describe "parseTextTag" $
    it "matches the specified tag" $
      testLine (testTag testTagData) (parseTextTag (G.GDTag "TEST"))
        `shouldBe` Match testTagData

  describe "parseListTag" $ do
    it "splits lists" $
      testLine (testTag "one,,two,three,four") (parseListTag (G.GDTag "TEST"))
        `shouldBe` Match ["one", "", "two", "three", "four"]
    it "handles the empty list" $
      testLine emptyTag (parseListTag (G.GDTag "TEST")) `shouldBe` Match []

  describe "parseWordTag" $ do
    it "parses numbers" $
      testLine (testTag "42") (parseWordTag (G.GDTag "TEST")) `shouldBe` Match 42
    it "doesn't parse negative numbers" $
      isError $ testLine (testTag "-42") (parseWordTag (G.GDTag "TEST"))
    it "doesn't parse empty strings" $
      isError $ testLine (testTag "") (parseWordTag (G.GDTag "TEST"))

  describe "parseBoolTag" $ do
    it "parses yes and no" $ do
      testLine (testTag "yes") (parseBoolTag (G.GDTag "TEST"))
        `shouldBe` Match True
      testLine (testTag "no") (parseBoolTag (G.GDTag "TEST"))
        `shouldBe` Match False
    it "parses case insensitive" $ do
      testLine (testTag "Yes") (parseBoolTag (G.GDTag "TEST"))
        `shouldBe` Match True
      testLine (testTag "nO") (parseBoolTag (G.GDTag "TEST"))
        `shouldBe` Match False
    it "doesn't parse anything else" $
      isError (testLine (testTag "true") (parseBoolTag (G.GDTag "TEST")))
        && isError (testLine (testTag "false") (parseBoolTag (G.GDTag "TEST")))
  where
    testLine l = parseStructure (G.GDTree <$> l <*> pure [])
