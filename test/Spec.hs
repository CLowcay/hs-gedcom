{-# LANGUAGE OverloadedStrings #-}

import Data.Gedcom.Internal.CoreTypes
import Data.Gedcom.Internal.LineParser
import Data.Gedcom.Internal.ParseMonads
import Data.Gedcom.Internal.Parser
import Data.Monoid
import Data.Void
import qualified Data.Text.All as T
import Test.Hspec
import Text.Megaparsec

testTagData :: T.Text
testTagData = "some test data"

testTag :: T.Text -> Maybe GDLine
testTag d = do
  (GDRoot [GDTree t []]) <- parseMaybe gdRoot$ "0 TEST " <> d <> "\n"
  return t

emptyTag :: Maybe GDLine
emptyTag = do
  (GDRoot [GDTree t []]) <- parseMaybe gdRoot$ "0 TEST\n"
  return t

ref1 :: GDXRefID
ref1 = GDXRefID "1"

linkTag :: Maybe GDLine
linkTag = do
  (GDRoot [GDTree t []]) <- parseMaybe gdRoot$ "0 TEST @1@\n"
  return t

linkLinkTag :: Maybe GDLine
linkLinkTag = do
  (GDRoot [GDTree t []]) <- parseMaybe gdRoot$ "0 @2@ TEST @1@\n"
  return t

crdTag :: Maybe GDLine
crdTag = do
  (GDRoot [GDTree t []]) <-
    parseMaybe gdRoot$ "0 @1@ TEST " <> testTagData <> "\n"
  return t

data ParseReturn a =
  Error T.Text | NoMatch | Match a deriving (Show, Eq)

isError :: ParseReturn a -> Bool
isError (Error _) = True
isError _ = False

parseStructure :: Maybe GDTree -> StructureParser a -> ParseReturn a
parseStructure Nothing p = Error "Line parse error"
parseStructure (Just v) p = case runStructure (p v) of
    (Right (Left _), _) -> NoMatch
    (Right (Right m), _) -> Match m
    (Left err, _) -> Error$ T.show err

noEscapes :: T.Text -> [(Maybe GDEscape, T.Text)]
noEscapes v = [(Nothing, v)]

main :: IO ()
main = hspec$ do
  describe "gdRoot"$ do
    it "parses a simple line with no children"$
      (testTag testTagData) `shouldBe`
        (Just$ GDLine (GDLevel 0) Nothing (GDTag "TEST")
          (Just . GDLineItemV . GDLineItem . noEscapes$ testTagData))
    it "parses a simple cross referenced line"$
      crdTag `shouldBe` (Just$ GDLine (GDLevel 0) (Just ref1) (GDTag "TEST")
        (Just . GDLineItemV . GDLineItem . noEscapes$ testTagData))
    it "parses a simple line cross referencing line"$
      linkTag `shouldBe` (Just$ GDLine (GDLevel 0) Nothing (GDTag "TEST")
        (Just . GDXRefIDV$  ref1))
    it "parses an empty tag with no children"$
      emptyTag `shouldBe`
        (Just$ GDLine (GDLevel 0) Nothing (GDTag "TEST") Nothing)

  describe "parseNoLinkTag"$ do
    it "matches the specified tag"$
      testLine (testTag testTagData) (parseNoLinkTag (GDTag "TEST") pure)
        `shouldBe` (Match (noEscapes testTagData, []))
    it "matches the specified tag (with a cross reference)" $
      testLine crdTag (parseNoLinkTag (GDTag "TEST") pure)
        `shouldBe` (Match (noEscapes testTagData, []))
    it "doesn't match other tags"$
      testLine (testTag testTagData) (parseNoLinkTag (GDTag "OTHER") pure)
        `shouldBe` NoMatch
    it "doesn't parse links"$
      isError$ testLine linkTag (parseNoLinkTag (GDTag "TEST") pure)

  describe "parseTag"$ do
    it "matches the specified tag"$
      testLine (testTag testTagData) (parseTag (GDTag "TEST") pure)
        `shouldBe` (Match . GDStructure$ (noEscapes testTagData, []))
    it "matches the specified tag (with a cross reference)" $
      testLine crdTag (parseTag (GDTag "TEST") pure)
        `shouldBe` (Match . GDStructure$ (noEscapes testTagData, []))
    it "doesn't match other tags"$
      testLine (testTag testTagData) (parseTag (GDTag "OTHER") pure)
        `shouldBe` NoMatch
    it "parses links"$
      testLine linkTag (parseTag (GDTag "TEST") pure)
        `shouldBe` (Match$ GDXRef ref1)
    it "doesn't parse links to links"$
      isError$ testLine linkLinkTag (parseTag (GDTag "TEST") pure)

  describe "parseLinkTag"$ do
    it "parses link tags"$
      testLine linkTag (parseLinkTag (GDTag "TEST")) `shouldBe`
        (Match$ (GDXRef ref1 :: GDRef Void))
    it "doesn't parse other tags"$
      isError$ testLine (testTag testTagData)
        (parseLinkTag (GDTag "TEST") :: StructureParser (GDRef Void))

  describe "parseTextTag"$ do
    it "matches the specified tag" $
      testLine (testTag testTagData) (parseTextTag (GDTag "TEST"))
        `shouldBe` (Match testTagData)

  describe "parseListTag"$ do
    it "splits lists"$
      testLine (testTag "one,,two,three,four") (parseListTag (GDTag "TEST"))
        `shouldBe` (Match ["one", "", "two", "three", "four"])
    it "handles the empty list"$
      testLine emptyTag (parseListTag (GDTag "TEST")) `shouldBe` (Match [])

  describe "parseWordTag"$ do
    it "parses numbers"$
      testLine (testTag "42") (parseWordTag (GDTag "TEST")) `shouldBe` (Match 42)
    it "doesn't parse negative numbers"$
      isError$ testLine (testTag "-42") (parseWordTag (GDTag "TEST"))
    it "doesn't parse empty strings"$
      isError$ testLine (testTag "") (parseWordTag (GDTag "TEST"))

  describe "parseBoolTag"$ do
    it "parses yes and no"$ do
      testLine (testTag "yes") (parseBoolTag (GDTag "TEST"))
        `shouldBe` (Match True)
      testLine (testTag "no") (parseBoolTag (GDTag "TEST"))
        `shouldBe` (Match False)
    it "parses case insensitive"$ do
      testLine (testTag "Yes") (parseBoolTag (GDTag "TEST"))
        `shouldBe` (Match True)
      testLine (testTag "nO") (parseBoolTag (GDTag "TEST"))
        `shouldBe` (Match False)
    it "doesn't parse anything else"$
      (isError$ testLine (testTag "true") (parseBoolTag (GDTag "TEST")))
      && (isError$ testLine (testTag "false") (parseBoolTag (GDTag "TEST")))

  where
    testLine l = parseStructure (GDTree <$> l <*> pure [])

