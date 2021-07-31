{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Data.Gedcom.Parser
-- Description: GEDCOM high level parsers
-- Copyright: (c) Callum Lowcay, 2017
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
--
-- These parsers extract the GEDCOM records from the raw syntax tree.
module Data.Gedcom.Internal.Parser
  ( parseGedcom,
    parseHeader,
    parseBoolTag,
    parseWordTag,
    parseTextTag,
    parseListTag,
    parseTag,
    parseLinkTag,
    parseNoLinkTag,
  )
where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Monad.Except (MonadError (throwError))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Dynamic (Dynamic, Typeable)
import Data.Gedcom.Internal.Common (Parser, dateExact, month, monthFr, monthHeb, showt, timeToPicos, timeValue, trim, yearGreg)
import qualified Data.Gedcom.Internal.CoreTypes as G
import Data.Gedcom.Internal.LineParser (gdDelim)
import Data.Gedcom.Internal.ParseMonads (MultiMonad, StructureMonad, StructureParser, addReference, parseMulti, parseOptional, parseRequired, runMultiMonad, runStructure)
import qualified Data.Gedcom.Structure as G
import Data.Map (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (UTCTime), secondsToDiffTime)
import Text.Megaparsec (MonadParsec (try), anySingle, count', many, parseMaybe)
import Text.Megaparsec.Char (digitChar, string')

-- | Parse a 'Gedcom' value from the raw GEDCOM syntax tree.
parseGedcom :: G.GDRoot -> (Either G.GDError G.Gedcom, Map G.GDXRefID Dynamic)
parseGedcom (G.GDRoot children) =
  runStructure . runMultiMonad children $
    G.Gedcom
      <$> parseRequired (G.GDTag "HEAD") parseHeader
      <*> parseMulti parseFamily
      <*> parseMulti (parseIndividual (G.GDTag "INDI"))
      <*> parseMulti parseMultimediaRecord
      <*> parseMulti parseNote
      <*> parseMulti parseRepository
      <*> parseMulti parseSource
      <*> parseMulti (parseSubmitter (G.GDTag "SUBM"))

-- | Parse a 'G.Header'.
parseHeader :: StructureParser G.Header
parseHeader = parseNoLinkTag (G.GDTag "HEAD") $ \(_, children) ->
  runMultiMonad children $
    G.Header
      <$> parseRequired (G.GDTag "SOUR") parseHeaderSource
      <*> parseOptional (parseTextTag (G.GDTag "DEST"))
      <*> parseOptional parseExactDateTime
      <*> parseRequired (G.GDTag "SUBM") (parseSubmitter (G.GDTag "SUBM"))
      <*> parseOptional parseSubmission
      <*> parseOptional parseFile
      <*> parseOptional parseCopyright
      <*> parseRequired (G.GDTag "GEDC") parseGedcomFormat
      <*> parseRequired (G.GDTag "CHAR") parseCharset
      <*> parseOptional parseLanguage
      <*> parseOptional parsePlaceForm
      <*> parseOptional (parseTextTag (G.GDTag "NOTE"))

-- | Parse a 'G.HeaderSource'.
parseHeaderSource :: StructureParser G.HeaderSource
parseHeaderSource = parseNoLinkTag (G.GDTag "SOUR") $ \(sid, children) ->
  runMultiMonad children $
    G.HeaderSource (G.gdIgnoreEscapes sid)
      <$> parseOptional parseVersion
      <*> parseOptional parseName
      <*> parseOptional parseCorp
      <*> parseOptional parseHeaderSourceData

-- | Parse a 'G.Corp'.
parseCorp :: StructureParser G.Corp
parseCorp = parseNoLinkTag (G.GDTag "COPR") $ \(name, children) ->
  runMultiMonad children $
    G.Corp (G.gdIgnoreEscapes name)
      <$> (parseContactDetails >>= (parseOptional . parseAddress))

-- | Parse a 'G.HeaderSourceData'.
parseHeaderSourceData :: StructureParser G.HeaderSourceData
parseHeaderSourceData = parseNoLinkTag (G.GDTag "DATA") $ \(name, children) ->
  runMultiMonad children $
    G.HeaderSourceData (G.gdIgnoreEscapes name)
      <$> parseOptional parseExactDate
      <*> parseOptional parseCopyright

-- | Parse a 'G.Family'.
parseFamily :: StructureParser (G.GDRef G.Family)
parseFamily = parseTag (G.GDTag "FAM") $ \(_, children) ->
  runMultiMonad children $
    G.Family
      <$> parseOptional parseRestrictionNotice
      <*> parseFamilyEvent
      <*> parseOptional (parseIndividual (G.GDTag "HUSB"))
      <*> parseOptional (parseIndividual (G.GDTag "WIFE"))
      <*> parseMulti (parseIndividual (G.GDTag "CHIL"))
      <*> parseOptional (parseWordTag (G.GDTag "NCHI"))
      <*> parseMulti (parseSubmitter (G.GDTag "SUBM"))
      <*> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseOptional parseChangeDate
      <*> parseMulti parseNote
      <*> parseMulti parseSourceCitation
      <*> parseMulti parseMultimedia

-- | Parse an 'G.Individual'.
parseIndividual :: G.GDTag -> StructureParser (G.GDRef G.Individual)
parseIndividual tag = parseTag tag $ \(_, children) ->
  runMultiMonad children $
    G.Individual
      <$> parseOptional parseRestrictionNotice
      <*> parseOptional parsePersonalName
      <*> parseOptional parseSex
      <*> parseIndividualEvent
      <*> parseIndividualAttribute
      <*> parseMulti parseChildToFamilyLink
      <*> parseMulti parseSpouseToFamilyLink
      <*> parseMulti (parseSubmitter (G.GDTag "SUBM"))
      <*> parseMulti parseAssociation
      <*> parseMulti (parseIndividual (G.GDTag "ALIA"))
      <*> parseMulti (parseSubmitter (G.GDTag "ANCI"))
      <*> parseMulti (parseSubmitter (G.GDTag "DECI"))
      <*> parseOptional parseRFN
      <*> parseOptional parseAFN
      <*> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseOptional parseChangeDate
      <*> parseMulti parseNote
      <*> parseMulti parseSourceCitation
      <*> parseMulti parseMultimedia

-- | Parse a 'G.Note'.
parseNote :: StructureParser (G.GDRef G.Note)
parseNote = parseTag (G.GDTag "NOTE") $ \(t, children) ->
  runMultiMonad children $
    G.Note (G.gdIgnoreEscapes t)
      <$> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseMulti parseSourceCitation
      <*> parseOptional parseChangeDate

-- | Parse a 'G.Repository'.
parseRepository :: StructureParser (G.GDRef G.Repository)
parseRepository = parseTag (G.GDTag "REPO") $ \(_, children) ->
  runMultiMonad children $
    G.Repository
      <$> parseRequired (G.GDTag "NAME") parseName
      <*> (parseContactDetails >>= (parseOptional . parseAddress))
      <*> parseMulti parseNote
      <*> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseOptional parseChangeDate

-- | Parse a 'G.Source'.
parseSource :: StructureParser (G.GDRef G.Source)
parseSource = parseTag (G.GDTag "SOUR") $ \(_, children) ->
  runMultiMonad children $
    G.Source
      <$> parseOptional parseSourceData
      <*> parseOptional (parseTextTag (G.GDTag "AUTH"))
      <*> parseOptional (parseTextTag (G.GDTag "TITL"))
      <*> parseOptional (parseTextTag (G.GDTag "ABBR"))
      <*> parseOptional (parseTextTag (G.GDTag "PUBL"))
      <*> parseOptional (parseTextTag (G.GDTag "TEXT"))
      <*> parseMulti parseRepositoryCitation
      <*> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseOptional parseChangeDate
      <*> parseMulti parseNote
      <*> parseMulti parseMultimedia

-- | Parse a 'G.Submitter'.
parseSubmitter :: G.GDTag -> StructureParser (G.GDRef G.Submitter)
parseSubmitter tag = parseTag tag $ \(_, children) ->
  runMultiMonad children $
    G.Submitter
      <$> parseRequired
        (G.GDTag "NAME")
        ((fmap . fmap . fmap) getPersonalName parseName)
      <*> (parseContactDetails >>= (parseOptional . parseAddress))
      <*> parseOptional parseMultimedia
      <*> parseMulti parseLanguage
      <*> parseOptional parseRFN
      <*> parseOptional parseRIN
      <*> parseMulti parseNote
      <*> parseOptional parseChangeDate

-- | Parse a 'G.Submission'.
parseSubmission :: StructureParser (G.GDRef G.Submission)
parseSubmission = parseTag (G.GDTag "SUBN") $ \(_, children) ->
  runMultiMonad children $
    G.Submission
      <$> parseOptional (parseSubmitter (G.GDTag "SUBM"))
      <*> parseOptional (parseTextTag (G.GDTag "FAMF"))
      <*> parseOptional (parseTextTag (G.GDTag "TEMP"))
      <*> parseOptional (parseWordTag (G.GDTag "ANCE"))
      <*> parseOptional (parseWordTag (G.GDTag "DESC"))
      <*> parseOptional (parseBoolTag (G.GDTag "ORDI"))
      <*> parseOptional parseRIN
      <*> parseMulti parseNote
      <*> parseOptional parseChangeDate

-- | Parse a 'G.RestrictionNotice'.
parseRestrictionNotice :: StructureParser G.RestrictionNotice
parseRestrictionNotice = parseNoLinkTag (G.GDTag "RESN") $ \(t, _) ->
  case parseMaybe parser (G.gdIgnoreEscapes t) of
    Nothing ->
      throwError . G.FormatError $
        "Bad restriction notice " <> showt t
    Just r -> pure r
  where
    parser :: Parser G.RestrictionNotice
    parser =
      (G.Confidential <$ "confidential")
        <|> (G.Locked <$ "locked")
        <|> (G.Privacy <$ "privacy")

-- | Parse a list of 'G.FamilyEvent's.
parseFamilyEvent :: MultiMonad [G.FamilyEvent]
parseFamilyEvent =
  concat
    <$> mapM
      (parseMulti . familyEventTag)
      [ (G.GDTag "ANUL", const G.Annuled),
        (G.GDTag "CENS", const G.FamCensus),
        (G.GDTag "DIV", const G.Divorce),
        (G.GDTag "DIVF", const G.DivorceFiled),
        (G.GDTag "ENGA", const G.Engagement),
        (G.GDTag "MARB", const G.MarriageBann),
        (G.GDTag "MARC", const G.MarriageContract),
        (G.GDTag "MARR", const G.Marriage),
        (G.GDTag "MARL", const G.MarriageLicense),
        (G.GDTag "MARS", const G.MarriageSettlement),
        (G.GDTag "RESI", const G.Residence),
        (G.GDTag "EVEN", G.FamilyEventType)
      ]
  where
    familyEventTag (tag, mkType) = parseNoLinkTag tag $ \(t, children) ->
      runMultiMonad children $
        G.FamilyEvent (mkType $ G.gdIgnoreEscapes t)
          <$> parseFamilyEventDetail

-- | Parse a 'G.FamiltyEventDetail'.
parseFamilyEventDetail :: MultiMonad G.FamilyEventDetail
parseFamilyEventDetail =
  G.FamilyEventDetail
    <$> parseOptional (parseAge (G.GDTag "HUSB"))
    <*> parseOptional (parseAge (G.GDTag "WIFE"))
    <*> parseEventDetail

-- | Parse an 'G.EventDetail'.
parseEventDetail :: MultiMonad G.EventDetail
parseEventDetail =
  G.EventDetail
    <$> parseOptional (parseTextTag (G.GDTag "TYPE"))
    <*> parseOptional parseDateValue
    <*> parseOptional parsePlace
    <*> (parseContactDetails >>= (parseOptional . parseAddress))
    <*> parseOptional (parseTextTag (G.GDTag "AGNC"))
    <*> parseOptional (parseTextTag (G.GDTag "RELI"))
    <*> parseOptional (parseTextTag (G.GDTag "CAUS"))
    <*> parseOptional parseRestrictionNotice
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation
    <*> parseMulti parseMultimedia

-- | Parse an AGE tag.
parseAge :: G.GDTag -> StructureParser Word
parseAge tag = parseNoLinkTag tag $ \(_, children) ->
  runMultiMonad children $
    parseRequired (G.GDTag "AGE") (parseWordTag (G.GDTag "AGE"))

-- | Parse a 'G.SourceData'.
parseSourceData :: StructureParser G.SourceData
parseSourceData = parseNoLinkTag (G.GDTag "DATA") $ \(_, children) ->
  runMultiMonad children $
    G.SourceData
      <$> parseMulti parseSourceRecordedEvent
      <*> parseOptional (parseTextTag (G.GDTag "AGNC"))
      <*> parseMulti parseNote

-- | Parse a 'G.Place'.
parsePlace :: StructureParser G.Place
parsePlace = parseNoLinkTag (G.GDTag "PLAC") $ \(t, children) ->
  runMultiMonad children $
    G.Place
      (T.splitOn "," . G.gdIgnoreEscapes $ t)
      <$> parseOptional (parseListTag (G.GDTag "FORM"))
      <*> parseOptional parsePhoneticPlaceName
      <*> parseOptional parseRomanPlaceName
      <*> parseOptional parseMapCoord
      <*> parseMulti parseNote

-- | Parse a 'G.PhoneticPlaceName'.
parsePhoneticPlaceName :: StructureParser G.PhoneticPlaceName
parsePhoneticPlaceName = parseNoLinkTag (G.GDTag "FONE") $ \(t, children) ->
  runMultiMonad children $
    G.PhoneticPlaceName
      <$> parseRequired (G.GDTag "TYPE") parsePhoneticType
      <*> (pure . T.splitOn "," . G.gdIgnoreEscapes $ t)

-- | Parse a 'G.RomanPlaceName'.
parseRomanPlaceName :: StructureParser G.RomanPlaceName
parseRomanPlaceName = parseNoLinkTag (G.GDTag "ROMN") $ \(t, children) ->
  runMultiMonad children $
    G.RomanPlaceName
      <$> parseRequired (G.GDTag "TYPE") parseRomanType
      <*> (pure . T.splitOn "," . G.gdIgnoreEscapes $ t)

-- | Parse a 'G.PhoneticType'.
parsePhoneticType :: StructureParser G.PhoneticType
parsePhoneticType = parseNoLinkTag (G.GDTag "TYPE") $ \(t, _) ->
  pure $ case trim . T.toUpper . G.gdIgnoreEscapes $ t of
    "HANGUL" -> G.Hangul
    "KANA" -> G.Kana
    v -> G.PhoneticType v

-- | Parse a 'G.RomanType'.
parseRomanType :: StructureParser G.RomanType
parseRomanType = parseNoLinkTag (G.GDTag "TYPE") $ \(t, _) ->
  pure $ case trim . T.toUpper . G.gdIgnoreEscapes $ t of
    "PINYIN" -> G.Pinyin
    "ROMAJI" -> G.Romaji
    "WADEGILES" -> G.WadeGiles
    v -> G.RomanType v

-- | Parse a 'G.MapCoord'.
parseMapCoord :: StructureParser G.MapCoord
parseMapCoord = parseNoLinkTag (G.GDTag "MAP") $ \(_, children) ->
  runMultiMonad children $
    G.MapCoord
      <$> parseRequired
        (G.GDTag "LATI")
        ((fmap . fmap) G.Longitude <$> parseLongLat (G.GDTag "LATI") 'N' 'S')
      <*> parseRequired
        (G.GDTag "LONG")
        ((fmap . fmap) G.Latitude <$> parseLongLat (G.GDTag "LONG") 'E' 'W')

-- | Parse a 'G.Longitude' or 'G.Latitude' value.
parseLongLat ::
  -- | The tag to parse
  G.GDTag ->
  -- | The single character prefix that indicates a positive value.
  Char ->
  -- | The single character prefix that indicates a negative value.
  Char ->
  StructureParser Double
parseLongLat tag p n = parseNoLinkTag tag $ \(t, _) ->
  case T.uncons . T.toUpper . G.gdIgnoreEscapes $ t of
    Nothing ->
      throwError . G.FormatError $
        "Badly formatted longitude/latitude " <> showt t
    Just (i, r)
      | i == p -> pure $ read . T.unpack $ r
      | i == n -> pure $ negate . read . T.unpack $ r
      | otherwise ->
        throwError . G.FormatError $
          "Badly formatted longitude/latitude" <> showt t

-- | Parse a 'G.PersonalName'.
parsePersonalName :: StructureParser G.PersonalName
parsePersonalName = parseNoLinkTag (G.GDTag "NAME") $ \(n, children) ->
  runMultiMonad children $
    G.PersonalName
      (getPersonalName . G.gdIgnoreEscapes $ n)
      <$> parseOptional parseNameType
      <*> parseNamePieces
      <*> parseMulti parsePhoneticName
      <*> parseMulti parseRomanName

-- | Extract a 'G.Name' from a string.
getPersonalName :: Text -> G.Name
getPersonalName = G.Name <$> reformat <*> (getMiddle . T.splitOn "/")
  where
    reformat = T.unwords . T.words . T.map (\c -> if c == '/' then ' ' else c)
    getMiddle [_, m, _] = Just $ trim m
    getMiddle _ = Nothing

-- | Parse a 'G.NameType'.
parseNameType :: StructureParser G.NameType
parseNameType = parseNoLinkTag (G.GDTag "TYPE") $ \(t', _) ->
  let t = G.gdIgnoreEscapes t'
   in pure . fromMaybe (G.NameType t) . parseMaybe parser $ t
  where
    parser :: Parser G.NameType
    parser =
      (G.AKA <$ "aka")
        <|> (G.BirthName <$ "birth")
        <|> (G.Immigrant <$ "immigrant")
        <|> (G.Maiden <$ "maiden")
        <|> (G.Married <$ "married")

-- | Parse a 'G.PersonalNamePieces' structure.
parseNamePieces :: MultiMonad G.PersonalNamePieces
parseNamePieces =
  G.PersonalNamePieces
    <$> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "NPFX")))
    <*> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "GIVN")))
    <*> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "NICK")))
    <*> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "SPFX")))
    <*> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "SURN")))
    <*> (fromMaybe [] <$> parseOptional (parseListTag (G.GDTag "NSFX")))
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation

-- | Parse a 'G.PhoneticName'.
parsePhoneticName :: StructureParser G.PhoneticName
parsePhoneticName = parseNoLinkTag (G.GDTag "FONE") $ \(t, children) ->
  runMultiMonad children $
    G.PhoneticName
      (getPersonalName . G.gdIgnoreEscapes $ t)
      <$> parseRequired (G.GDTag "TYPE") parsePhoneticType
      <*> parseNamePieces

-- | Parse a 'G.RomanizedName'.
parseRomanName :: StructureParser G.RomanizedName
parseRomanName = parseNoLinkTag (G.GDTag "FONE") $ \(t, children) ->
  runMultiMonad children $
    G.RomanizedName
      (getPersonalName . G.gdIgnoreEscapes $ t)
      <$> parseRequired (G.GDTag "TYPE") parseRomanType
      <*> parseNamePieces

-- | Parse a 'G.Sex'.
parseSex :: StructureParser G.Sex
parseSex = parseNoLinkTag (G.GDTag "SEX") $ \(t, _) ->
  case trim . T.toUpper . G.gdIgnoreEscapes $ t of
    "M" -> pure G.Male
    "F" -> pure G.Female
    "U" -> pure G.Undetermined
    _ -> throwError . G.FormatError $ "Unknown sex code " <> showt t

-- | Parse a list of 'G.IndividualAttribute's.
parseIndividualAttribute :: MultiMonad [G.IndividualAttribute]
parseIndividualAttribute =
  concat
    <$> mapM
      (parseMulti . attributeTag)
      [ (G.GDTag "CAST", G.Caste),
        (G.GDTag "DSCR", G.PhysicalDescription),
        (G.GDTag "EDUC", G.Education),
        (G.GDTag "IPNO", G.NationalID),
        (G.GDTag "NATI", G.NationalOrigin),
        (G.GDTag "NCHI", G.NChildren . read . T.unpack),
        (G.GDTag "NMR", G.NMarriages . read . T.unpack),
        (G.GDTag "OCCU", G.Occupation),
        (G.GDTag "PROP", G.Possessions),
        (G.GDTag "RELI", G.Religion),
        (G.GDTag "RESI", const G.ResidesAt),
        (G.GDTag "SSN", G.SocialSecurity),
        (G.GDTag "TITL", G.Title),
        (G.GDTag "FACT", G.Fact)
      ]
  where
    attributeTag (tag, mkType) = parseNoLinkTag tag $ \(t, children) ->
      runMultiMonad children $
        G.IndividualAttribute (mkType $ G.gdIgnoreEscapes t)
          <$> parseIndividualEventDetail

-- | Parse a list of 'G.IndividualEvent's
parseIndividualEvent :: MultiMonad [G.IndividualEvent]
parseIndividualEvent = do
  famc <-
    concat
      <$> mapM
        (parseMulti . famcTag)
        [(G.GDTag "BIRT", G.Birth), (G.GDTag "CHR", G.Christening)]
  adop <- parseMulti . parseNoLinkTag (G.GDTag "ADOP") $ \(_, children) ->
    runMultiMonad children $
      G.IndividualEvent
        <$> (G.Adoption <$> parseOptional parseAdopFamilyRef)
        <*> parseIndividualEventDetail
  simple <-
    concat
      <$> mapM
        (parseMulti . eventTag)
        [ (G.GDTag "DEAT", const G.Death),
          (G.GDTag "BURI", const G.Burial),
          (G.GDTag "CREM", const G.Cremation),
          (G.GDTag "BAPM", const G.Baptism),
          (G.GDTag "BARM", const G.BarMitzvah),
          (G.GDTag "BASM", const G.BasMitzvah),
          (G.GDTag "BLES", const G.Blessing),
          (G.GDTag "CHRA", const G.ChristeningAdult),
          (G.GDTag "CONF", const G.Confirmation),
          (G.GDTag "FCOM", const G.FirstCommunion),
          (G.GDTag "ORDN", const G.Ordination),
          (G.GDTag "NATU", const G.Naturalization),
          (G.GDTag "EMIG", const G.Emigration),
          (G.GDTag "IMMI", const G.Immigration),
          (G.GDTag "CENS", const G.IndvCensus),
          (G.GDTag "PROB", const G.Probate),
          (G.GDTag "WILL", const G.Will),
          (G.GDTag "GRAD", const G.Graduation),
          (G.GDTag "RETI", const G.Retirement),
          (G.GDTag "EVEN", G.IndividualEventType)
        ]
  pure $ famc ++ adop ++ simple
  where
    eventTag (tag, mkType) = parseNoLinkTag tag $ \(t, children) ->
      runMultiMonad children $
        G.IndividualEvent (mkType $ G.gdIgnoreEscapes t)
          <$> parseIndividualEventDetail
    famcTag (tag, mkType) = parseNoLinkTag tag $ \(_, children) ->
      runMultiMonad children $
        G.IndividualEvent
          <$> (mkType <$> parseOptional parseFamilyRef)
          <*> parseIndividualEventDetail
    parseFamilyRef = parseLinkTag (G.GDTag "FAMC")
    parseAdopFamilyRef = parseTagFull (G.GDTag "FAMC") nullrref $ \(lb, children) ->
      case lb of
        Right _ -> throwError . G.RequiredRef $ "Missing link in FAMC"
        Left f ->
          runMultiMonad children $
            G.AdoptionDetail f <$> parseOptional parseParent
    parseParent = parseNoLinkTag (G.GDTag "ADOP") $ \(t, _) ->
      case trim . T.toUpper . G.gdIgnoreEscapes $ t of
        "HUSB" -> pure G.Husband
        "WIFE" -> pure G.Wife
        "BOTH" -> pure G.BothParents
        _ -> throwError . G.FormatError $ "Invalid parent " <> showt t

-- | Parse an 'G.IndividualEventDetail'.
parseIndividualEventDetail :: MultiMonad G.IndividualEventDetail
parseIndividualEventDetail =
  G.IndividualEventDetail
    <$> parseEventDetail
    <*> parseOptional (parseWordTag (G.GDTag "AGE"))

-- | Parse a 'G.ChildToFamilyLink'.
parseChildToFamilyLink :: StructureParser G.ChildToFamilyLink
parseChildToFamilyLink = parseTagFull (G.GDTag "FAMC") nullrref $ \(lb, children) ->
  case lb of
    Right _ -> throwError . G.RequiredRef $ "Missing link in FAMC"
    Left f ->
      runMultiMonad children $
        G.ChildToFamilyLink f
          <$> parseOptional parsePedigree
          <*> parseOptional parseChildLinkStatus
          <*> parseMulti parseNote

-- | Parse a 'G.Pedigree'.
parsePedigree :: StructureParser G.Pedigree
parsePedigree = parseNoLinkTag (G.GDTag "PEDI") $ \(t, _) ->
  case trim . T.toUpper . G.gdIgnoreEscapes $ t of
    "ADOPTED" -> pure G.Adopted
    "BIRTH" -> pure G.ByBirth
    "FOSTER" -> pure G.Foster
    "SEALING" -> pure G.Sealing
    _ -> throwError . G.FormatError $ "Invalid pedigree code " <> showt t

-- | Parse a 'G.ChildLinkStatus'.
parseChildLinkStatus :: StructureParser G.ChildLinkStatus
parseChildLinkStatus = parseNoLinkTag (G.GDTag "STAT") $ \(t, _) ->
  case trim . T.toUpper . G.gdIgnoreEscapes $ t of
    "CHALLENGED" -> pure G.Challenged
    "DISPROVEN" -> pure G.Disproved
    "PROVEN" -> pure G.Proven
    _ -> throwError . G.FormatError $ "Invalid child link status " <> showt t

-- | Parse a 'G.SpouseToFamilyLink'.
parseSpouseToFamilyLink :: StructureParser G.SpouseToFamilyLink
parseSpouseToFamilyLink = parseTagFull (G.GDTag "FAMS") nullrref $
  \(lb, children) ->
    case lb of
      Right _ -> throwError . G.RequiredRef $ "Missing link in FAMS"
      Left f ->
        runMultiMonad children $
          G.SpouseToFamilyLink f
            <$> parseMulti parseNote

-- | Parse an 'G.Association'.
parseAssociation :: StructureParser G.Association
parseAssociation = parseTagFull (G.GDTag "ASSO") nullrref $ \(lb, children) ->
  case lb of
    Right _ -> throwError . G.RequiredRef $ "Missing link in ASSO"
    Left i ->
      runMultiMonad children $
        G.Association i
          <$> parseRequired (G.GDTag "RELA") (parseTextTag (G.GDTag "RELA"))
          <*> parseMulti parseSourceCitation
          <*> parseMulti parseNote

-- | Parse a 'G.SourceRecordedEvent'.
parseSourceRecordedEvent :: StructureParser G.SourceRecordedEvent
parseSourceRecordedEvent = parseNoLinkTag (G.GDTag "EVEN") $
  \(recorded, children) ->
    runMultiMonad children $
      G.SourceRecordedEvent
        (mapMaybe getEventType . T.splitOn "," . G.gdIgnoreEscapes $ recorded)
        <$> parseOptional fullParseDatePeriod
        <*> parseOptional (parseListTag (G.GDTag "PLAC"))
  where
    fullParseDatePeriod = parseNoLinkTag (G.GDTag "DATE") $ \(date, _) ->
      case parseDatePeriod date of
        Just v -> pure v
        Nothing ->
          throwError . G.FormatError $
            "Badly formatted date period: " <> showt date

-- | Parse a 'G.RepositoryCitation'.
parseRepositoryCitation :: StructureParser G.RepositoryCitation
parseRepositoryCitation = parseTagFull (G.GDTag "REPO") nullrref $
  \(lb, children) ->
    let repo = case lb of
          Left v -> Just v
          Right _ -> Nothing
     in runMultiMonad children $
          G.RepositoryCitation repo
            <$> parseMulti parseNote
            <*> parseOptional parseCallNumber

-- | Parse a 'G.CallNumber'.
parseCallNumber :: StructureParser G.CallNumber
parseCallNumber = parseNoLinkTag (G.GDTag "CALN") $ \(n, children) ->
  runMultiMonad children $
    G.CallNumber (G.gdIgnoreEscapes n)
      <$> parseOptional (parseMultimediaType (G.GDTag "MEDI"))

-- | Parse a 'G.SourceCitation'.
parseSourceCitation :: StructureParser G.SourceCitation
parseSourceCitation = parseTagFull (G.GDTag "SOUR") nullrref $
  \(lb, children) ->
    case lb of
      Left source ->
        runMultiMonad children $
          G.SourceCitation (Right source)
            <$> parseOptional (parseTextTag (G.GDTag "PAGE"))
            <*> parseMulti parseMultimedia
            <*> parseMulti parseNote
            <*> parseOptional parseQuality
      Right description ->
        runMultiMonad children $
          G.SourceCitation
            <$> ( Left . G.SourceDescription (G.gdIgnoreEscapes description)
                    <$> parseMulti (parseTextTag (G.GDTag "TEXT"))
                )
            <*> pure Nothing
            <*> parseMulti parseMultimedia
            <*> parseMulti parseNote
            <*> parseOptional parseQuality

-- | Parse a 'G.GedcomFormat'.
parseGedcomFormat :: StructureParser G.GedcomFormat
parseGedcomFormat = parseNoLinkTag (G.GDTag "GEDC") $ \(_, children) ->
  runMultiMonad children $
    G.GedcomFormat
      <$> parseRequired (G.GDTag "VERS") parseVersion
      <*> parseRequired (G.GDTag "FORM") parseGedcomForm

-- | Parse a 'G.GedcomForm'.
parseGedcomForm :: StructureParser G.GedcomForm
parseGedcomForm = parseNoLinkTag (G.GDTag "FORM") $ \(t, _) ->
  pure $
    if (T.toUpper . G.gdIgnoreEscapes $ t) == "LINEAGE-LINKED"
      then G.GedcomLineageLinked
      else G.GedcomUnsupported (G.gdIgnoreEscapes t)

-- | Parse a 'G.Charset'.
parseCharset :: StructureParser G.Charset
parseCharset = parseNoLinkTag (G.GDTag "CHAR") $ \(cs, children) ->
  runMultiMonad children $
    G.Charset (G.gdIgnoreEscapes cs)
      <$> parseOptional parseVersion

-- | Parse a 'G.ChangeDate'.
parseChangeDate :: StructureParser G.ChangeDate
parseChangeDate = parseNoLinkTag (G.GDTag "CHAN") $ \(_, children) ->
  runMultiMonad children $
    G.ChangeDate
      <$> parseRequired (G.GDTag "DATE") parseExactDateTime
      <*> parseOptional parseNote

-- | Parse 'G.ContactDetails'.
parseContactDetails :: MultiMonad G.ContactDetails
parseContactDetails =
  G.ContactDetails
    <$> parseMulti (parseTextTag (G.GDTag "PHON"))
    <*> parseMulti (parseTextTag (G.GDTag "EMAIL"))
    <*> parseMulti (parseTextTag (G.GDTag "FAX"))
    <*> parseMulti (parseTextTag (G.GDTag "WWW"))

-- | Parse an 'G.Address'.
parseAddress :: G.ContactDetails -> StructureParser G.Address
parseAddress contacts = parseNoLinkTag (G.GDTag "ADDR") $ \(addr, children) ->
  runMultiMonad children $
    G.Address (G.gdIgnoreEscapes addr)
      <$> parseOptional (parseTextTag (G.GDTag "CITY"))
      <*> parseOptional (parseTextTag (G.GDTag "STAE"))
      <*> parseOptional (parseTextTag (G.GDTag "POS"))
      <*> parseOptional (parseTextTag (G.GDTag "CTRY"))
      <*> pure contacts

-- | Parse a 'G.DateValue'.
parseDateValue :: StructureParser G.DateValue
parseDateValue = parseNoLinkTag (G.GDTag "DATE") $ \(t, _) ->
  let date =
        G.DateApproxV <$> parseDateApprox t
          <|> (G.DateRangeV <$> parseDateRange t)
          <|> (G.DatePeriodV <$> parseDatePeriod t)
          <|> parseDatePhrase t
          <|> (G.DateV <$> parseDate t)
   in case date of
        Just x -> pure x
        Nothing -> throwError . G.FormatError $ "Invalid date format " <> showt t

-- | Decode a calendar escape sequence to a 'G.Calendar'.
decodeCalendarEscape :: Maybe G.GDEscape -> G.Calendar
decodeCalendarEscape Nothing = G.Gregorian
decodeCalendarEscape (Just (G.GDEscape c)) = case T.toUpper c of
  "DGREGORIAN" -> G.Gregorian
  "DJULIAN" -> G.Julian
  "DHEBREW" -> G.Hebrew
  "DFRENCH" -> G.French
  "DROMAN" -> G.Julian
  "DUNKNOWN" -> G.Gregorian
  _ -> G.Gregorian

-- | Prepare text for parsing a 'G.DateValue'.
prepareDateText :: [(Maybe G.GDEscape, Text)] -> [(Maybe G.GDEscape, Text)]
prepareDateText =
  G.gdFilterEscapes
    [ G.GDEscape "DGREGORIAN",
      G.GDEscape "DJULIAN",
      G.GDEscape "DHEBREW",
      G.GDEscape "DFRENCH",
      G.GDEscape "DROMAN",
      G.GDEscape "DUNKNOWN"
    ]

-- | Attempt to extract a 'G.DatePeriod' from a string with escape sequences.
parseDatePeriod :: [(Maybe G.GDEscape, Text)] -> Maybe G.DatePeriod
parseDatePeriod t = case prepareDateText t of
  ((mCalendarEscape1, t1') : rest) ->
    let calendar1 = decodeCalendarEscape mCalendarEscape1
        t1 = trim $ T.toUpper t1'
        from = trim <$> T.stripPrefix "FROM" t1
        to = trim <$> T.stripPrefix "TO" t1
     in case to of
          Just r -> G.DateTo <$> getDate calendar1 r
          Nothing -> case from of
            Nothing -> Nothing
            Just r ->
              let topcs = T.splitOn "TO" r
               in case topcs of
                    [mfdate, mto] ->
                      G.DateFrom
                        <$> getDate calendar1 (trim mfdate)
                        <*> pure (getDate calendar1 (trim mto))
                    [mfdate] -> case rest of
                      [] ->
                        G.DateFrom
                          <$> getDate calendar1 (trim mfdate) <*> pure Nothing
                      ((mCalendarEscape2, t2') : _) ->
                        let calendar2 = decodeCalendarEscape mCalendarEscape2
                            t2 = trim $ T.toUpper t2'
                            to2 = trim <$> T.stripPrefix "TO" t2
                         in case to2 of
                              Nothing -> Nothing
                              Just r2 ->
                                G.DateFrom
                                  <$> getDate calendar1 (trim mfdate)
                                  <*> pure (getDate calendar2 (trim r2))
                    _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'G.DateRange' from a string with escape sequences.
parseDateRange :: [(Maybe G.GDEscape, Text)] -> Maybe G.DateRange
parseDateRange t = case prepareDateText t of
  ((mCalendarEscape1, t1) : rest) ->
    let calendar1 = decodeCalendarEscape mCalendarEscape1
        (pref, date1) = second trim . T.splitAt 3 . T.toUpper . trim $ t1
     in case pref of
          "BEF" -> G.DateBefore <$> getDate calendar1 date1
          "AFT" -> G.DateAfter <$> getDate calendar1 date1
          "BET" -> case T.splitOn "AND" date1 of
            [d1, date2'] ->
              G.DateBetween
                <$> getDate calendar1 d1
                <*> getDate calendar1 (trim date2')
            [d1] -> case rest of
              [] -> Nothing
              ((mCalendarEscape2, t2) : _) ->
                let calendar2 = decodeCalendarEscape mCalendarEscape2
                 in G.DateBetween
                      <$> getDate calendar1 d1
                      <*> getDate calendar2 (trim t2)
            _ -> Nothing
          _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'G.DateApprox' from a string with escape sequences.
parseDateApprox :: [(Maybe G.GDEscape, Text)] -> Maybe G.DateApprox
parseDateApprox t = case prepareDateText t of
  ((mCalendarEscape1, t1) : _) ->
    let calendar1 = decodeCalendarEscape mCalendarEscape1
        (pref, date) = second trim . T.splitAt 3 . T.toUpper . trim $ t1
        cons = case pref of
          "ABT" -> Just G.DateAbout
          "CAL" -> Just G.DateCalculated
          "EST" -> Just G.DateEstimated
          _ -> Nothing
     in cons <*> getDate calendar1 date
  _ -> Nothing

-- | Attempt to extract a 'G.DatePhrase' from a string with escape sequences.
parseDatePhrase :: [(Maybe G.GDEscape, Text)] -> Maybe G.DateValue
parseDatePhrase t = case prepareDateText t of
  ((mCalendarEscape1, t1) : _) ->
    let calendar1 = decodeCalendarEscape mCalendarEscape1
        int = trim <$> T.stripPrefix "INT" t1
        opp = trim <$> T.stripPrefix "(" t1
     in case int of
          Nothing -> case opp of
            Nothing -> Nothing
            Just phrase ->
              Just $
                G.DatePhrase Nothing (trim . T.dropEnd 1 $ phrase)
          Just rest -> case trim <$> T.splitOn "(" rest of
            [date, phrase] ->
              Just $
                G.DatePhrase (getDate calendar1 date) (trim . T.dropEnd 1 $ phrase)
            _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'G.Date' from a string with escape sequences.
parseDate :: [(Maybe G.GDEscape, Text)] -> Maybe G.Date
parseDate t = case prepareDateText t of
  ((mCalendarEscape, cal) : rest) ->
    let calendar = decodeCalendarEscape mCalendarEscape
     in getDate calendar (cal <> G.gdIgnoreEscapes rest)
  _ -> Nothing

-- | Parse a 'G.Date' from a given string assuming the given 'Calendar'.
getDate :: G.Calendar -> Text -> Maybe G.Date
getDate calendar = parseMaybe parser
  where
    yearParser = case calendar of
      G.Gregorian -> yearGreg
      _ -> read <$> count' 1 4 digitChar
    parseYear =
      (\y bc -> G.Year $ if bc then negate y else y)
        <$> yearParser <*> ((True <$ "B.C.") <|> pure False)
    parser :: Parser G.Date
    parser =
      try
        ( G.Date calendar
            <$> (Just . read <$> count' 1 2 digitChar)
            <*> (gdDelim >> (Just <$> parseMonth))
            <*> (gdDelim >> parseYear)
        )
        <|> try
          ( G.Date calendar Nothing
              <$> (gdDelim >> (Just <$> parseMonth))
              <*> (gdDelim >> parseYear)
          )
        <|> ( G.Date calendar Nothing Nothing
                <$> (gdDelim >> parseYear)
            )
    parseMonth = case calendar of
      G.Gregorian -> month
      G.Julian -> month
      G.Hebrew -> monthHeb
      G.French -> monthFr

-- | Parse an exact date.
parseExactDate :: StructureParser UTCTime
parseExactDate = parseNoLinkTag (G.GDTag "DATE") $ \(date, _) ->
  case parseMaybe dateExact (G.gdIgnoreEscapes date) of
    Nothing -> throwError . G.FormatError $ "Bad date \"" <> showt date <> "\""
    Just (d, m, y) ->
      pure $
        UTCTime
          (fromGregorian (fromIntegral y) (fromIntegral m) d)
          (secondsToDiffTime 0)

-- | Parse an exact date and time.
parseExactDateTime :: StructureParser UTCTime
parseExactDateTime = parseNoLinkTag (G.GDTag "DATE") $ \(date, children) -> do
  mtime <- runMultiMonad children $ parseOptional (parseTextTag (G.GDTag "TIME"))

  dt <- case mtime of
    Nothing -> pure $ secondsToDiffTime 0
    Just t -> case parseMaybe timeValue t of
      Nothing -> throwError . G.FormatError $ "Bad time \"" <> showt t <> "\""
      Just Nothing -> pure $ secondsToDiffTime 0
      Just (Just time) -> pure $ timeToPicos time

  case parseMaybe dateExact (G.gdIgnoreEscapes date) of
    Nothing -> throwError . G.FormatError $ "Bad date \"" <> showt date <> "\""
    Just (d, m, y) ->
      pure $
        UTCTime
          (fromGregorian (fromIntegral y) (fromIntegral m) d)
          dt

-- | Parse a 'G.Multimedia' record.
parseMultimediaRecord :: StructureParser (G.GDRef G.Multimedia)
parseMultimediaRecord = parseMultimediaRaw (G.GDTag "TYPE")

-- | Parse a 'G.Multimedia' link.
parseMultimedia :: StructureParser (G.GDRef G.Multimedia)
parseMultimedia = parseMultimediaRaw (G.GDTag "MEDI")

-- | Actual parser for 'G.Multimedia' values.
parseMultimediaRaw :: G.GDTag -> StructureParser (G.GDRef G.Multimedia)
parseMultimediaRaw typeTag = parseTag (G.GDTag "OBJE") $ \(_, children) ->
  runMultiMonad children $
    G.Multimedia
      <$> ( parseOptional (parseMultimediaFormat typeTag)
              >>= (parseMulti . parseMultimediaFile typeTag)
          )
      <*> parseOptional (parseTextTag (G.GDTag "TITL"))
      <*> parseMulti parseUserReference
      <*> parseOptional parseRIN
      <*> parseMulti parseNote
      <*> parseMulti parseSourceCitation
      <*> parseOptional parseChangeDate

-- | Parse a 'G.MultimediaFile'.
parseMultimediaFile ::
  G.GDTag ->
  Maybe G.MultimediaFormat ->
  StructureParser G.MultimediaFile
parseMultimediaFile typeTag mf = parseNoLinkTag (G.GDTag "FILE") $
  \(name, children) -> do
    (mc, title) <-
      runMultiMonad children $
        (,)
          <$> parseOptional (parseMultimediaFormat typeTag)
          <*> parseOptional (parseTextTag (G.GDTag "TITL"))
    case mc <|> mf of
      Nothing -> throwError . G.TagError $ "Missing FORM tag for file format"
      Just c -> pure $ G.MultimediaFile (G.gdIgnoreEscapes name) c title

-- | Parse a 'G.MultimediaFormat'.
parseMultimediaFormat :: G.GDTag -> StructureParser G.MultimediaFormat
parseMultimediaFormat tag = parseNoLinkTag (G.GDTag "FORM") $ \(v', children) ->
  let v = G.gdIgnoreEscapes v'
   in runMultiMonad children $
        G.MultimediaFormat
          (fromMaybe (G.MF_OTHER v) $ parseMaybe parser v)
          <$> parseOptional (parseMultimediaType tag)
  where
    parser :: Parser G.MultimediaFileFormat
    parser =
      (G.MF_BMP <$ "bmp")
        <|> (G.MF_GIF <$ "gif")
        <|> (G.MF_JPG <$ "jpg")
        <|> (G.MF_OLE <$ "ole")
        <|> (G.MF_PCX <$ "pcx")
        <|> (G.MF_TIF <$ "tif")
        <|> (G.MF_WAV <$ "wav")

-- | Parse a 'G.MultimediaType'.
parseMultimediaType :: G.GDTag -> StructureParser G.MultimediaType
parseMultimediaType tag = parseNoLinkTag tag $ \(v', _) ->
  let v = G.gdIgnoreEscapes v'
   in pure . fromMaybe (G.MT_OTHER v) $ parseMaybe parser v
  where
    parser :: Parser G.MultimediaType
    parser =
      (G.MT_AUDIO <$ "audio")
        <|> (G.MT_BOOK <$ "book")
        <|> (G.MT_CARD <$ "card")
        <|> (G.MT_ELECTRONIC <$ "electronic")
        <|> (G.MT_FICHE <$ "fiche")
        <|> (G.MT_FILM <$ "film")
        <|> (G.MT_MAGAZINE <$ "magazine")
        <|> (G.MT_MANUSCRIPT <$ "manuscript")
        <|> (G.MT_MAP <$ "map")
        <|> (G.MT_NEWSPAPER <$ "newspaper")
        <|> (G.MT_PHOTO <$ "photo")
        <|> (G.MT_TOMBSTONE <$ "tombstone")
        <|> (G.MT_VIDEO <$ "video")

-- | Parse a 'G.FamilyEventType'.
getFamilyEventType :: Text -> Maybe G.FamilyEventType
getFamilyEventType = parseMaybe parser
  where
    parser :: Parser G.FamilyEventType
    parser =
      (G.Annuled <$ "ANUL")
        <|> (G.FamCensus <$ "CENS")
        <|> (G.Divorce <$ "DIV")
        <|> (G.DivorceFiled <$ "DIVF")
        <|> (G.Engagement <$ "ENGA")
        <|> (G.MarriageBann <$ "MARB")
        <|> (G.MarriageContract <$ "MARC")
        <|> (G.Marriage <$ "MARR")
        <|> (G.MarriageLicense <$ "MARL")
        <|> (G.MarriageSettlement <$ "MARS")
        <|> (G.Residence <$ "RESI")
        <|> (G.FamilyEventType . T.pack <$> ("EVEN" *> gdDelim *> Text.Megaparsec.many anySingle))

-- | Parse an 'G.IndividualEventType'.
getIndividualEventType :: Text -> Maybe G.IndividualEventType
getIndividualEventType = parseMaybe parser
  where
    parser :: Parser G.IndividualEventType
    parser =
      (G.Birth Nothing <$ ("BIRTH" *> optional (gdDelim >> "Y")))
        <|> (G.Christening Nothing <$ ("CHR" *> optional (gdDelim >> "Y")))
        <|> (G.Death <$ ("DEAT" *> optional (gdDelim >> "Y")))
        <|> (G.Burial <$ "BURI")
        <|> (G.Cremation <$ "CREM")
        <|> (G.Adoption Nothing <$ "ADOP")
        <|> (G.Baptism <$ "BAPM")
        <|> (G.BarMitzvah <$ "BARM")
        <|> (G.BasMitzvah <$ "BASM")
        <|> (G.Blessing <$ "BLES")
        <|> (G.ChristeningAdult <$ "CHRA")
        <|> (G.Confirmation <$ "CONF")
        <|> (G.FirstCommunion <$ "FCOM")
        <|> (G.Ordination <$ "ORDN")
        <|> (G.Naturalization <$ "NATU")
        <|> (G.Emigration <$ "EMIG")
        <|> (G.Immigration <$ "IMMI")
        <|> (G.IndvCensus <$ "CENS")
        <|> (G.Probate <$ "PROB")
        <|> (G.Will <$ "WILL")
        <|> (G.Graduation <$ "GRAD")
        <|> (G.Retirement <$ "RETI")
        <|> ( G.IndividualEventType . T.pack
                <$> ("EVEN" *> gdDelim *> Text.Megaparsec.many anySingle)
            )

-- | Parse an 'G.EventType'.
getEventType :: Text -> Maybe G.EventType
getEventType et =
  let t = trim $ T.toUpper et
      fam = getFamilyEventType et
      indv = getIndividualEventType et
   in if t == "CENS"
        then Just G.Census
        else
          if T.take 4 t == "EVEN"
            then Just . G.EventType . trim . T.drop 4 $ et
            else fmap G.FamilyEventTypeV fam <|> fmap G.IndividualEventTypeV indv

-- | Parse a place form structure.
parsePlaceForm :: StructureParser [Text]
parsePlaceForm = parseNoLinkTag (G.GDTag "PLAC") $ \(_, children) ->
  runMultiMonad children $
    maybe [] (T.splitOn ",")
      <$> parseOptional (parseTextTag (G.GDTag "FORM"))

-- | Parse a 'G.UserReference'.
parseUserReference :: StructureParser G.UserReference
parseUserReference = parseNoLinkTag (G.GDTag "REFN") $ \(i, children) ->
  runMultiMonad children $
    G.UserReference (G.gdIgnoreEscapes i)
      <$> parseOptional (parseTextTag (G.GDTag "TYPE"))

-- | Parse a version structure.
parseVersion :: StructureParser Text
parseVersion = parseTextTag (G.GDTag "VERS")

-- | Parse a NAME tag.
parseName :: StructureParser Text
parseName = parseTextTag (G.GDTag "NAME")

-- | Parse a copyright tag.
parseCopyright :: StructureParser Text
parseCopyright = parseTextTag (G.GDTag "COPR")

-- | Parse a file name.
parseFile :: StructureParser FilePath
parseFile = (fmap . fmap) T.unpack <$> parseTextTag (G.GDTag "FILE")

-- | Parse an 'G.AFN'.
parseAFN :: StructureParser G.AFN
parseAFN = (fmap . fmap) G.AFN <$> parseTextTag (G.GDTag "AFN")

-- | Parse a 'G.RFN'.
parseRFN :: StructureParser G.RFN
parseRFN = (fmap . fmap) G.RFN <$> parseTextTag (G.GDTag "RFN")

-- | Parse a 'G.RIN'.
parseRIN :: StructureParser G.RIN
parseRIN = (fmap . fmap) G.RIN <$> parseTextTag (G.GDTag "RIN")

-- | Parse a 'G.Language'.
parseLanguage :: StructureParser G.Language
parseLanguage = (fmap . fmap) G.Language <$> parseTextTag (G.GDTag "LANG")

-- | Parse a 'G.QualityAssessment'.
parseQuality :: StructureParser G.QualityAssessment
parseQuality = (fmap . fmap) G.QualityAssessment <$> parseWordTag (G.GDTag "QUAY")

-- | Parse a boolean value.
parseBoolTag :: G.GDTag -> StructureParser Bool
parseBoolTag tag = parseNoLinkTag tag $ \(v, _) ->
  case parseMaybe ynParser (G.gdIgnoreEscapes v) of
    Nothing -> throwError . G.FormatError $ "Expected boolean, saw " <> showt v
    Just yn -> pure yn
  where
    ynParser :: Parser Bool
    ynParser = (True <$ string' "yes") <|> (False <$ string' "no")

-- | Parse a Word value.
parseWordTag :: G.GDTag -> StructureParser Word
parseWordTag tag = parseNoLinkTag tag $ \(v, _) ->
  case parseMaybe parser (G.gdIgnoreEscapes v) of
    Nothing -> throwError . G.FormatError $ "Expected number, saw " <> showt v
    Just n -> pure . read $ n
  where
    parser :: Parser String
    parser = Text.Megaparsec.many digitChar

-- | Extract the text from a tag.
parseTextTag :: G.GDTag -> StructureParser Text
parseTextTag tag = parseNoLinkTag tag (pure . G.gdIgnoreEscapes . fst)

-- | Extract a list of comma separated values from a tag.
parseListTag :: G.GDTag -> StructureParser [Text]
parseListTag tag =
  parseNoLinkTag tag (pure . split . trim . G.gdIgnoreEscapes . fst)
  where
    split "" = []
    split s = T.splitOn "," s

-- | Handler for tags that cannot contain cross references.
type NoLinkHandler a =
  ([(Maybe G.GDEscape, Text)], [G.GDTree]) -> StructureMonad a

-- | Handler for general tags.
type TagHandler b a =
  (Either (G.GDRef b) [(Maybe G.GDEscape, Text)], [G.GDTree]) ->
  StructureMonad a

-- | A function that adds references to the cross reference table.
type RegisterRef a = G.GDTag -> G.GDXRefID -> a -> StructureMonad ()

-- | Don't add any references.
nullrref :: RegisterRef a
nullrref _ _ _ = pure ()

-- | Add direct references only.
defrref :: Typeable a => RegisterRef (G.GDRef a)
defrref _ thisID (G.GDStructure a) = addReference thisID a
defrref tag _ _ =
  throwError . G.UnexpectedRef $
    "Referenced structure references another structure " <> showt tag

-- | Parse a tag which is either a GEDCOM structure, or a reference to the
-- expected GEDCOM structure.
parseTag ::
  Typeable a =>
  -- | The tag to parse.
  G.GDTag ->
  -- | A handler for the tag
  NoLinkHandler a ->
  StructureParser (G.GDRef a)
parseTag tag handler = parseTagFull tag defrref $ \(lb, children) ->
  case lb of
    Left ref -> pure ref
    Right text -> G.GDStructure <$> handler (text, children)

-- | Parse a tag which cannot contain a cross reference (i.e. the tag must
-- contain the structure itself, not a reference to another structure).
parseNoLinkTag ::
  -- | The tag to parse.
  G.GDTag ->
  -- | A handler for the tag.
  NoLinkHandler a ->
  StructureParser a
parseNoLinkTag tag handler = parseTagFull tag nullrref $ \(lb, children) ->
  case lb of
    Left _ ->
      throwError . G.UnexpectedRef $
        "Cannot follow cross references on " <> showt tag
    Right text -> handler (text, children)

-- | Parse a tag which must contain a cross reference to another structure, not
-- the structure itself.
parseLinkTag :: G.GDTag -> StructureParser (G.GDRef a)
parseLinkTag tag = parseTagFull tag nullrref $ \(lb, _) ->
  case lb of
    Left ref -> pure ref
    Right _ ->
      throwError . G.RequiredRef $
        "Expected cross reference was missing in " <> showt tag

-- | The most general tag parsing function.
parseTagFull ::
  -- | The tag to parse.
  G.GDTag ->
  -- | How to register cross references.
  RegisterRef a ->
  -- | How to convert the tag into a GEDCOM structure.
  TagHandler b a ->
  StructureParser a
parseTagFull tag rref handler t@(G.GDTree (G.GDLine _ mthisID tag' v) children) =
  if tag /= tag'
    then pure $ Left t
    else do
      r <- case v of
        Nothing -> Right <$> (handler . first Right $ parseCont mempty children)
        Just (G.GDXRefIDV xref) ->
          Right <$> handler (Left (G.GDXRef xref), children)
        Just (G.GDLineItemV l1) ->
          Right <$> (handler . first Right $ parseCont l1 children)
      case (mthisID, r) of
        (Just thisID, Right rv) -> rref tag thisID rv
        _ -> pure ()
      pure r

-- | Handle CONT and CONC tags.
parseCont ::
  -- | The value of the first line
  G.GDLineItem ->
  -- | The sub tree
  [G.GDTree] ->
  -- | The concatenation of the values of all the CONT and CONC tags, and the remaining tags.
  ([(Maybe G.GDEscape, Text)], [G.GDTree])
parseCont l1 children =
  bimap assemble (fmap snd) . span (isJust . fst) $
    zipWith (\l r -> (cont l <|> conc l, r)) children children
  where
    assemble = G.gdLineData . mconcat . (l1 :) . mapMaybe fst
    cont (G.GDTree (G.GDLine _ _ (G.GDTag "CONT") (Just (G.GDLineItemV l))) _) =
      Just $ G.GDLineItem [(Nothing, "\r\n")] <> l
    cont _ = Nothing
    conc (G.GDTree (G.GDLine _ _ (G.GDTag "CONC") (Just (G.GDLineItemV l))) _) =
      Just $ G.gdTrimLineItem l
    conc _ = Nothing
