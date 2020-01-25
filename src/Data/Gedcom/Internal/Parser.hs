{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Data.Gedcom.Parser
Description: GEDCOM high level parsers
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC

These parsers extract the GEDCOM records from the raw syntax tree.

-}
module Data.Gedcom.Internal.Parser (
  parseGedcom,
  parseHeader,

  parseBoolTag,
  parseWordTag,
  parseTextTag,
  parseListTag,
  parseTag,
  parseLinkTag,
  parseNoLinkTag
) where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor
import Data.Dynamic
import Data.Gedcom.Internal.Common
import Data.Gedcom.Internal.CoreTypes
import Data.Gedcom.Internal.LineParser
import Data.Gedcom.Internal.ParseMonads
import Data.Gedcom.Structure
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Map.Lazy as M
import qualified Data.Text.All as T
import Text.Megaparsec
import Text.Megaparsec.Char

-- | Parse a 'Gedcom' value from the raw GEDCOM syntax tree.
parseGedcom :: GDRoot -> (Either GDError Gedcom, M.Map GDXRefID Dynamic)
parseGedcom (GDRoot children) =
  runStructure.runMultiMonad children$ Gedcom
    <$> parseRequired (GDTag "HEAD") parseHeader
    <*> parseMulti parseFamily
    <*> parseMulti (parseIndividual (GDTag "INDI"))
    <*> parseMulti parseMultimediaRecord
    <*> parseMulti parseNote
    <*> parseMulti parseRepository
    <*> parseMulti parseSource
    <*> parseMulti (parseSubmitter (GDTag "SUBM"))

-- | Parse a 'Header'.
parseHeader :: StructureParser Header
parseHeader = parseNoLinkTag (GDTag "HEAD")$ \(_, children) ->
  runMultiMonad children$ Header
    <$> parseRequired (GDTag "SOUR") parseHeaderSource
    <*> parseOptional (parseTextTag (GDTag "DEST"))
    <*> parseOptional parseExactDateTime
    <*> parseRequired (GDTag "SUBM") (parseSubmitter (GDTag "SUBM"))
    <*> parseOptional parseSubmission
    <*> parseOptional parseFile
    <*> parseOptional parseCopyright
    <*> parseRequired (GDTag "GEDC") parseGedcomFormat
    <*> parseRequired (GDTag "CHAR") parseCharset
    <*> parseOptional parseLanguage
    <*> parseOptional parsePlaceForm
    <*> parseOptional (parseTextTag (GDTag "NOTE"))

-- | Parse a 'HeaderSource'.
parseHeaderSource :: StructureParser HeaderSource
parseHeaderSource = parseNoLinkTag (GDTag "SOUR")$ \(sid, children) ->
  runMultiMonad children$ HeaderSource (gdIgnoreEscapes sid)
    <$> parseOptional parseVersion
    <*> parseOptional parseName
    <*> parseOptional parseCorp
    <*> parseOptional parseHeaderSourceData

-- | Parse a 'Corp'.
parseCorp :: StructureParser Corp
parseCorp = parseNoLinkTag (GDTag "COPR")$ \(name, children) ->
  runMultiMonad children$ Corp (gdIgnoreEscapes name)
    <$> (parseContactDetails >>= (parseOptional.parseAddress))

-- | Parse a 'HeaderSourceData'.
parseHeaderSourceData :: StructureParser HeaderSourceData
parseHeaderSourceData = parseNoLinkTag (GDTag "DATA")$ \(name, children) ->
  runMultiMonad children$ HeaderSourceData (gdIgnoreEscapes name)
    <$> parseOptional parseExactDate
    <*> parseOptional parseCopyright

-- | Parse a 'Family'.
parseFamily :: StructureParser (GDRef Family)
parseFamily = parseTag (GDTag "FAM")$ \(_, children) ->
  runMultiMonad children$ Family
    <$> parseOptional parseRestrictionNotice
    <*> parseFamilyEvent
    <*> parseOptional (parseIndividual (GDTag "HUSB"))
    <*> parseOptional (parseIndividual (GDTag "WIFE"))
    <*> parseMulti (parseIndividual (GDTag "CHIL"))
    <*> parseOptional (parseWordTag (GDTag "NCHI"))
    <*> parseMulti (parseSubmitter (GDTag "SUBM"))
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseOptional parseChangeDate
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation
    <*> parseMulti parseMultimedia

-- | Parse an 'Individual'.
parseIndividual :: GDTag -> StructureParser (GDRef Individual)
parseIndividual tag = parseTag tag$ \(_, children) ->
  runMultiMonad children$ Individual
    <$> parseOptional parseRestrictionNotice
    <*> parseOptional parsePersonalName
    <*> parseOptional parseSex
    <*> parseIndividualEvent
    <*> parseIndividualAttribute
    <*> parseMulti parseChildToFamilyLink
    <*> parseMulti parseSpouseToFamilyLink
    <*> parseMulti (parseSubmitter (GDTag "SUBM"))
    <*> parseMulti parseAssociation
    <*> parseMulti (parseIndividual (GDTag "ALIA"))
    <*> parseMulti (parseSubmitter (GDTag "ANCI"))
    <*> parseMulti (parseSubmitter (GDTag "DECI"))
    <*> parseOptional parseRFN
    <*> parseOptional parseAFN
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseOptional parseChangeDate
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation
    <*> parseMulti parseMultimedia

-- | Parse a 'Node'.
parseNote :: StructureParser (GDRef Note)
parseNote = parseTag (GDTag "NOTE")$ \(t, children) ->
  runMultiMonad children$ Note (gdIgnoreEscapes t)
    <$> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseMulti parseSourceCitation
    <*> parseOptional parseChangeDate

-- | Parse a 'Repository'.
parseRepository :: StructureParser (GDRef Repository)
parseRepository = parseTag (GDTag "REPO")$ \(_, children) ->
  runMultiMonad children$ Repository
    <$> parseRequired (GDTag "NAME") parseName
    <*> (parseContactDetails >>= (parseOptional.parseAddress))
    <*> parseMulti parseNote
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseOptional parseChangeDate

-- | Parse a 'Source'.
parseSource :: StructureParser (GDRef Source)
parseSource = parseTag (GDTag "SOUR")$ \(_, children) ->
  runMultiMonad children$ Source
    <$> parseOptional parseSourceData
    <*> parseOptional (parseTextTag (GDTag "AUTH"))
    <*> parseOptional (parseTextTag (GDTag "TITL"))
    <*> parseOptional (parseTextTag (GDTag "ABBR"))
    <*> parseOptional (parseTextTag (GDTag "PUBL"))
    <*> parseOptional (parseTextTag (GDTag "TEXT"))
    <*> parseMulti parseRepositoryCitation
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseOptional parseChangeDate
    <*> parseMulti parseNote
    <*> parseMulti parseMultimedia

-- | Parse a 'Submitter'.
parseSubmitter :: GDTag -> StructureParser (GDRef Submitter)
parseSubmitter tag = parseTag tag$ \(_, children) ->
  runMultiMonad children$ Submitter
    <$> parseRequired (GDTag "NAME")
      ((fmap.fmap.fmap) getPersonalName parseName)
    <*> (parseContactDetails >>= (parseOptional.parseAddress))
    <*> parseOptional parseMultimedia
    <*> parseMulti parseLanguage
    <*> parseOptional parseRFN
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseOptional parseChangeDate

-- | Parse a 'Submission'.
parseSubmission :: StructureParser (GDRef Submission)
parseSubmission = parseTag (GDTag "SUBN")$ \(_, children) ->
  runMultiMonad children$ Submission
    <$> parseOptional (parseSubmitter (GDTag "SUBM"))
    <*> parseOptional (parseTextTag (GDTag "FAMF"))
    <*> parseOptional (parseTextTag (GDTag "TEMP"))
    <*> parseOptional (parseWordTag (GDTag "ANCE"))
    <*> parseOptional (parseWordTag (GDTag "DESC"))
    <*> parseOptional (parseBoolTag (GDTag "ORDI"))
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseOptional parseChangeDate

-- | Parse a 'RestrictionNotice'.
parseRestrictionNotice :: StructureParser RestrictionNotice
parseRestrictionNotice = parseNoLinkTag (GDTag "RESN")$ \(t, _) ->
  case parseMaybe parser (gdIgnoreEscapes t) of
    Nothing -> throwError.FormatError$
      "Bad restriction notice " <> T.show t
    Just r -> return r
  where parser :: Parser RestrictionNotice
        parser =     (Confidential <$ "confidential")
                 <|> (Locked <$ "locked")
                 <|> (Privacy <$ "privacy")

-- | Parse a list of 'FamilyEvent's.
parseFamilyEvent :: MultiMonad [FamilyEvent]
parseFamilyEvent =
    concat <$> mapM (parseMulti.familyEventTag) [
      (GDTag "ANUL", const Annuled),
      (GDTag "CENS", const FamCensus),
      (GDTag "DIV",  const Divorce),
      (GDTag "DIVF", const DivorceFiled),
      (GDTag "ENGA", const Engagement),
      (GDTag "MARB", const MarriageBann),
      (GDTag "MARC", const MarriageContract),
      (GDTag "MARR", const Marriage),
      (GDTag "MARL", const MarriageLicense),
      (GDTag "MARS", const MarriageSettlement),
      (GDTag "RESI", const Residence),
      (GDTag "EVEN", FamilyEventType)]
  where
    familyEventTag (tag, mkType) = parseNoLinkTag tag$ \(t, children) ->
      runMultiMonad children$ FamilyEvent (mkType$ gdIgnoreEscapes t)
        <$> parseFamilyEventDetail

-- | Parse a 'FamiltyEventDetail'.
parseFamilyEventDetail :: MultiMonad FamilyEventDetail
parseFamilyEventDetail = FamilyEventDetail
  <$> parseOptional (parseAge (GDTag "HUSB"))
  <*> parseOptional (parseAge (GDTag "WIFE"))
  <*> parseEventDetail

-- | Parse an 'EventDetail'.
parseEventDetail :: MultiMonad EventDetail
parseEventDetail = EventDetail
  <$> parseOptional (parseTextTag (GDTag "TYPE"))
  <*> parseOptional parseDateValue
  <*> parseOptional parsePlace
  <*> (parseContactDetails >>= (parseOptional.parseAddress))
  <*> parseOptional (parseTextTag (GDTag "AGNC"))
  <*> parseOptional (parseTextTag (GDTag "RELI"))
  <*> parseOptional (parseTextTag (GDTag "CAUS"))
  <*> parseOptional parseRestrictionNotice
  <*> parseMulti parseNote
  <*> parseMulti parseSourceCitation
  <*> parseMulti parseMultimedia

-- | Parse an AGE tag.
parseAge :: GDTag -> StructureParser Word
parseAge tag = parseNoLinkTag tag$ \(_, children) ->
  runMultiMonad children$
    parseRequired (GDTag "AGE") (parseWordTag (GDTag "AGE"))

-- | Parse a 'SourceData'.
parseSourceData :: StructureParser SourceData
parseSourceData = parseNoLinkTag (GDTag "DATA")$ \(_, children) ->
  runMultiMonad children$ SourceData
    <$> parseMulti parseSourceRecordedEvent
    <*> parseOptional (parseTextTag (GDTag "AGNC"))
    <*> parseMulti parseNote

-- | Parse a 'Place'.
parsePlace :: StructureParser Place
parsePlace = parseNoLinkTag (GDTag "PLAC")$ \(t, children) ->
  runMultiMonad children$ Place
    <$> pure (T.splitOn "," . gdIgnoreEscapes$ t)
    <*> parseOptional (parseListTag (GDTag "FORM"))
    <*> parseOptional parsePhoneticPlaceName
    <*> parseOptional parseRomanPlaceName
    <*> parseOptional parseMapCoord
    <*> parseMulti parseNote

-- | Parse a 'PhoneticPlaceName'.
parsePhoneticPlaceName :: StructureParser PhoneticPlaceName
parsePhoneticPlaceName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ PhoneticPlaceName
    <$> parseRequired (GDTag "TYPE") parsePhoneticType
    <*> (pure . T.splitOn "," . gdIgnoreEscapes$ t)

-- | Parse a 'RomanPlaceName'.
parseRomanPlaceName :: StructureParser RomanPlaceName
parseRomanPlaceName = parseNoLinkTag (GDTag "ROMN")$ \(t, children) ->
  runMultiMonad children$ RomanPlaceName
    <$> parseRequired (GDTag "TYPE") parseRomanType
    <*> (pure . T.splitOn "," . gdIgnoreEscapes$ t)

-- | Parse a 'PhoneticType'.
parsePhoneticType :: StructureParser PhoneticType
parsePhoneticType = parseNoLinkTag (GDTag "TYPE")$ \(t, _) ->
  return$ case trim . T.toUpper . gdIgnoreEscapes$ t of
    "HANGUL" -> Hangul
    "KANA" -> Kana
    v -> PhoneticType v

-- | Parse a 'RomanType'.
parseRomanType :: StructureParser RomanType
parseRomanType = parseNoLinkTag (GDTag "TYPE")$ \(t, _) ->
  return$ case trim . T.toUpper . gdIgnoreEscapes$ t of
    "PINYIN" -> Pinyin
    "ROMAJI" -> Romaji
    "WADEGILES" -> WadeGiles
    v -> RomanType v

-- | Parse a 'MapCoord'.
parseMapCoord :: StructureParser MapCoord
parseMapCoord = parseNoLinkTag (GDTag "MAP")$ \(_, children) ->
  runMultiMonad children$ MapCoord
    <$> parseRequired (GDTag "LATI") ((fmap.fmap) Longitude <$>
      parseLongLat (GDTag "LATI") 'N' 'S')
    <*> parseRequired (GDTag "LONG") ((fmap.fmap) Latitude <$>
      parseLongLat (GDTag "LONG") 'E' 'W')

-- | Parse a 'Longitude' or 'Latitude' value.
parseLongLat ::
     GDTag -- ^ The tag to parse
  -> Char  -- ^ The single character prefix that indicates a positive value.  
  -> Char  -- ^ The single character prefix that indicates a negative value.
  -> StructureParser Double
parseLongLat tag p n = parseNoLinkTag tag$ \(t, _) ->
  case T.uncons . T.toUpper . gdIgnoreEscapes$ t of
    Nothing -> throwError.FormatError$
      "Badly formatted longitude/latitude " <> T.show t
    Just (i, r)
      | i == p -> return$ read . T.unpack$ r
      | i == n -> return$ negate . read . T.unpack$ r
      | otherwise -> throwError.FormatError$
        "Badly formatted longitude/latitude" <> T.show t

-- | Parse a 'PersonalName'.
parsePersonalName :: StructureParser PersonalName
parsePersonalName = parseNoLinkTag (GDTag "NAME")$ \(n, children) ->
  runMultiMonad children$ PersonalName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ n)
    <*> parseOptional parseNameType
    <*> parseNamePieces
    <*> parseMulti parsePhoneticName
    <*> parseMulti parseRomanName
    
-- | Extract a 'Name' from a string.
getPersonalName :: T.Text -> Name
getPersonalName = Name <$> reformat <*> (getMiddle . T.splitOn "/")
  where
   reformat = T.unwords . T.words . T.map (\c -> if c == '/' then ' ' else c)
   getMiddle [_, m, _] = Just$ trim m
   getMiddle _ = Nothing

-- | Parse a 'NameType'.
parseNameType :: StructureParser NameType
parseNameType = parseNoLinkTag (GDTag "TYPE")$ \(t', _) ->
  let t = gdIgnoreEscapes t'
  in return . fromMaybe (NameType t) . parseMaybe parser $ t
  where parser :: Parser NameType
        parser =     (AKA <$ "aka")
                 <|> (BirthName <$ "birth")
                 <|> (Immigrant <$ "immigrant")
                 <|> (Maiden <$ "maiden")
                 <|> (Married <$ "married")

-- | Parse a 'PersonalNamePieces' structure.
parseNamePieces :: MultiMonad PersonalNamePieces
parseNamePieces = PersonalNamePieces
  <$> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "NPFX")))
  <*> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "GIVN")))
  <*> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "NICK")))
  <*> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "SPFX")))
  <*> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "SURN")))
  <*> (fromMaybe [] <$> parseOptional (parseListTag (GDTag "NSFX")))
  <*> parseMulti parseNote
  <*> parseMulti parseSourceCitation

-- | Parse a 'PhoneticName'.
parsePhoneticName :: StructureParser PhoneticName
parsePhoneticName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ PhoneticName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ t)
    <*> parseRequired (GDTag "TYPE") parsePhoneticType
    <*> parseNamePieces

-- | Parse a 'RomanizedName'.
parseRomanName :: StructureParser RomanizedName
parseRomanName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ RomanizedName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ t)
    <*> parseRequired (GDTag "TYPE") parseRomanType
    <*> parseNamePieces

-- | Parse a 'Sex'.
parseSex :: StructureParser Sex
parseSex = parseNoLinkTag (GDTag "SEX")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes $ t of
    "M" -> return Male
    "F" -> return Female
    "U" -> return Undetermined
    _ -> throwError.FormatError$ "Unknown sex code " <> T.show t
  
-- | Parse a list of 'IndividualAttribute's.
parseIndividualAttribute :: MultiMonad [IndividualAttribute]
parseIndividualAttribute =
    concat <$> mapM (parseMulti.attributeTag) [
      (GDTag "CAST", Caste),
      (GDTag "DSCR", PhysicalDescription),
      (GDTag "EDUC",  Education),
      (GDTag "IPNO", NationalID),
      (GDTag "NATI", NationalOrigin),
      (GDTag "NCHI", NChildren . read . T.unpack),
      (GDTag "NMR", NMarriages . read . T.unpack),
      (GDTag "OCCU", Occupation),
      (GDTag "PROP", Possessions),
      (GDTag "RELI", Religion),
      (GDTag "RESI", const ResidesAt),
      (GDTag "SSN", SocialSecurity),
      (GDTag "TITL", Title),
      (GDTag "FACT", Fact)]
  where
    attributeTag (tag, mkType) = parseNoLinkTag tag$ \(t, children) ->
      runMultiMonad children$ IndividualAttribute (mkType$ gdIgnoreEscapes t)
        <$> parseIndividualEventDetail

-- | Parse a list of 'IndividualEvent's
parseIndividualEvent :: MultiMonad [IndividualEvent]
parseIndividualEvent = do
    famc <- concat <$> mapM (parseMulti.famcTag)
      [(GDTag "BIRT", Birth), (GDTag "CHR", Christening)]
    adop <- parseMulti.parseNoLinkTag (GDTag "ADOP")$ \(_, children) ->
      runMultiMonad children$ IndividualEvent
        <$> (Adoption <$> parseOptional parseAdopFamilyRef)
        <*> parseIndividualEventDetail
    simple <- concat <$> mapM (parseMulti.eventTag) [
      (GDTag "DEAT", const Death),
      (GDTag "BURI", const Burial),
      (GDTag "CREM", const Cremation),
      (GDTag "BAPM", const Baptism),
      (GDTag "BARM", const BarMitzvah),
      (GDTag "BASM", const BasMitzvah),
      (GDTag "BLES", const Blessing),
      (GDTag "CHRA", const ChristeningAdult),
      (GDTag "CONF", const Confirmation),
      (GDTag "FCOM", const FirstCommunion),
      (GDTag "ORDN", const Ordination),
      (GDTag "NATU", const Naturalization),
      (GDTag "EMIG", const Emigration),
      (GDTag "IMMI", const Immigration),
      (GDTag "CENS", const IndvCensus),
      (GDTag "PROB", const Probate),
      (GDTag "WILL", const Will),
      (GDTag "GRAD", const Graduation),
      (GDTag "RETI", const Retirement),
      (GDTag "EVEN", IndividualEventType)]
    return$ famc ++ adop ++ simple
  where
    eventTag (tag, mkType) = parseNoLinkTag tag$ \(t, children) ->
      runMultiMonad children$ IndividualEvent (mkType$ gdIgnoreEscapes t)
        <$> parseIndividualEventDetail
    famcTag (tag, mkType) = parseNoLinkTag tag$ \(_, children) ->
      runMultiMonad children$ IndividualEvent
        <$> (mkType <$> parseOptional parseFamilyRef)
        <*> parseIndividualEventDetail
    parseFamilyRef = parseLinkTag (GDTag "FAMC") 
    parseAdopFamilyRef = parseTagFull (GDTag "FAMC") nullrref$ \(lb, children) ->
      case lb of
        Right _ -> throwError.RequiredRef$ "Missing link in FAMC"
        Left f -> runMultiMonad children$
          AdoptionDetail f <$> parseOptional parseParent
    parseParent = parseNoLinkTag (GDTag "ADOP")$ \(t, _) ->
      case trim . T.toUpper . gdIgnoreEscapes $ t of
        "HUSB" -> return Husband
        "WIFE" -> return Wife
        "BOTH" -> return BothParents
        _ -> throwError.FormatError$ "Invalid parent " <> T.show t

-- | Parse an 'IndividualEventDetail'.
parseIndividualEventDetail :: MultiMonad IndividualEventDetail
parseIndividualEventDetail = IndividualEventDetail
  <$> parseEventDetail
  <*> parseOptional (parseWordTag (GDTag "AGE"))

-- | Parse a 'ChildToFamilyLink'.
parseChildToFamilyLink :: StructureParser ChildToFamilyLink
parseChildToFamilyLink = parseTagFull (GDTag "FAMC") nullrref$ \(lb, children) ->
  case lb of
    Right _ -> throwError.RequiredRef$ "Missing link in FAMC"
    Left f -> runMultiMonad children$ ChildToFamilyLink f
      <$> parseOptional parsePedigree
      <*> parseOptional parseChildLinkStatus
      <*> parseMulti parseNote

-- | Parse a 'Pedigree'.
parsePedigree :: StructureParser Pedigree
parsePedigree = parseNoLinkTag (GDTag "PEDI")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes$ t of
    "ADOPTED" -> return Adopted
    "BIRTH" -> return ByBirth
    "FOSTER" -> return Foster
    "SEALING" -> return Sealing
    _ -> throwError.FormatError$ "Invalid pedigree code " <> T.show t

-- | Parse a 'ChildLinkStatus'.
parseChildLinkStatus :: StructureParser ChildLinkStatus
parseChildLinkStatus = parseNoLinkTag (GDTag "STAT")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes$ t of
    "CHALLENGED" -> return Challenged
    "DISPROVEN" -> return Disproved
    "PROVEN" -> return Proven
    _ -> throwError.FormatError$ "Invalid child link status " <> T.show t

-- | Parse a 'SpouseToFamilyLink'.
parseSpouseToFamilyLink :: StructureParser SpouseToFamilyLink
parseSpouseToFamilyLink = parseTagFull (GDTag "FAMS") nullrref$
  \(lb, children) ->
    case lb of
      Right _ -> throwError.RequiredRef$ "Missing link in FAMS"
      Left f -> runMultiMonad children$ SpouseToFamilyLink f
        <$> parseMulti parseNote

-- | Parse an 'Association'.
parseAssociation :: StructureParser Association
parseAssociation = parseTagFull (GDTag "ASSO") nullrref$ \(lb, children) ->
  case lb of
    Right _ -> throwError.RequiredRef$ "Missing link in ASSO"
    Left i -> runMultiMonad children$ Association i
      <$> parseRequired (GDTag "RELA") (parseTextTag (GDTag "RELA"))
      <*> parseMulti parseSourceCitation
      <*> parseMulti parseNote

-- | Parse a 'SourceRecordedEvent'.
parseSourceRecordedEvent :: StructureParser SourceRecordedEvent
parseSourceRecordedEvent = parseNoLinkTag (GDTag "EVEN")$
  \(recorded, children) ->
   runMultiMonad children$ SourceRecordedEvent
     <$> (pure . catMaybes . fmap getEventType
       . T.splitOn "," . gdIgnoreEscapes$ recorded)
     <*> parseOptional fullParseDatePeriod
     <*> parseOptional (parseListTag (GDTag "PLAC"))
     where
      fullParseDatePeriod = parseNoLinkTag (GDTag "DATE")$ \(date, _) ->
        case parseDatePeriod date of
          Just v -> return v
          Nothing -> throwError.FormatError$
            "Badly formatted date period: " <> T.show date

-- | Parse a 'RepositoryCitation'.
parseRepositoryCitation :: StructureParser RepositoryCitation
parseRepositoryCitation = parseTagFull (GDTag "REPO") nullrref$
  \(lb, children) ->
    let repo = case lb of
                 Left v -> Just v
                 Right _ -> Nothing
    in runMultiMonad children$ RepositoryCitation repo
      <$> parseMulti parseNote
      <*> parseOptional parseCallNumber

-- | Parse a 'CallNumber'.
parseCallNumber :: StructureParser CallNumber
parseCallNumber = parseNoLinkTag (GDTag "CALN")$ \(n, children) ->
  runMultiMonad children$ CallNumber (gdIgnoreEscapes n)
    <$> parseOptional (parseMultimediaType (GDTag "MEDI"))

-- | Parse a 'SourceCitation'.
parseSourceCitation :: StructureParser SourceCitation
parseSourceCitation = parseTagFull (GDTag "SOUR") nullrref$
  \(lb, children) ->
    case lb of
      Left source ->
        runMultiMonad children$ SourceCitation (Right source)
          <$> parseOptional (parseTextTag (GDTag "PAGE"))
          <*> parseMulti parseMultimedia
          <*> parseMulti parseNote
          <*> parseOptional parseQuality
      Right description ->
        runMultiMonad children$ SourceCitation
          <$> (Left . SourceDescription (gdIgnoreEscapes description)
            <$> parseMulti (parseTextTag (GDTag "TEXT")))
          <*> pure Nothing
          <*> parseMulti parseMultimedia
          <*> parseMulti parseNote
          <*> parseOptional parseQuality

-- | Parse a 'GedcomFormat'.
parseGedcomFormat :: StructureParser GedcomFormat
parseGedcomFormat = parseNoLinkTag (GDTag "GEDC")$ \(_, children) ->
  runMultiMonad children$ GedcomFormat
    <$> parseRequired (GDTag "VERS") parseVersion
    <*> parseRequired (GDTag "FORM") parseGedcomForm

-- | Parse a 'GedcomForm'.
parseGedcomForm :: StructureParser GedcomForm
parseGedcomForm = parseNoLinkTag (GDTag "FORM")$ \(t, _) ->
  return$ if (T.toUpper . gdIgnoreEscapes$ t) == "LINEAGE-LINKED"
    then GedcomLineageLinked
    else GedcomUnsupported (gdIgnoreEscapes t)

-- | Parse a 'Charset'.
parseCharset :: StructureParser Charset
parseCharset = parseNoLinkTag (GDTag "CHAR")$ \(cs, children) ->
  runMultiMonad children$ Charset (gdIgnoreEscapes cs)
    <$> parseOptional parseVersion

-- | Parse a 'ChangeDate'.
parseChangeDate :: StructureParser ChangeDate
parseChangeDate = parseNoLinkTag (GDTag "CHAN")$ \(_, children) ->
  runMultiMonad children$ ChangeDate
    <$> parseRequired (GDTag "DATE") parseExactDateTime
    <*> parseOptional parseNote

-- | Parse 'ContactDetails'.
parseContactDetails :: MultiMonad ContactDetails
parseContactDetails = ContactDetails
  <$> parseMulti (parseTextTag (GDTag "PHON"))
  <*> parseMulti (parseTextTag (GDTag "EMAIL"))
  <*> parseMulti (parseTextTag (GDTag "FAX"))
  <*> parseMulti (parseTextTag (GDTag "WWW"))

-- | Parse an 'Address'.
parseAddress :: ContactDetails -> StructureParser Address
parseAddress contacts = parseNoLinkTag (GDTag "ADDR")$ \(addr, children) ->
  runMultiMonad children$ Address (gdIgnoreEscapes addr)
    <$> parseOptional (parseTextTag (GDTag "CITY"))
    <*> parseOptional (parseTextTag (GDTag "STAE"))
    <*> parseOptional (parseTextTag (GDTag "POS"))
    <*> parseOptional (parseTextTag (GDTag "CTRY"))
    <*> pure contacts

-- | Parse a 'DateValue'.
parseDateValue :: StructureParser DateValue
parseDateValue = parseNoLinkTag (GDTag "DATE")$ \(t, _) ->
  let date = DateApproxV <$> parseDateApprox t
         <|> (DateRangeV <$> parseDateRange t)
         <|> (DatePeriodV <$> parseDatePeriod t)
         <|> parseDatePhrase t
         <|> (DateV <$> parseDate t)
  in case date of
    Just x -> return x
    Nothing -> throwError.FormatError$ "Invalid date format " <> T.show t

-- | Decode a calendar escape sequence to a 'Calendar'.
decodeCalendarEscape :: Maybe GDEscape -> Calendar
decodeCalendarEscape Nothing = Gregorian
decodeCalendarEscape (Just (GDEscape c)) = case T.toUpper c of
                                             "DGREGORIAN" -> Gregorian
                                             "DJULIAN" -> Julian
                                             "DHEBREW" -> Hebrew
                                             "DFRENCH" -> French
                                             "DROMAN" -> Julian
                                             "DUNKNOWN" -> Gregorian
                                             _ -> Gregorian

-- | Prepare text for parsing a 'DateValue'.
prepareDateText :: [(Maybe GDEscape, T.Text)] -> [(Maybe GDEscape, T.Text)]
prepareDateText = gdFilterEscapes [
                    GDEscape "DGREGORIAN",
                    GDEscape "DJULIAN",
                    GDEscape "DHEBREW",
                    GDEscape "DFRENCH",
                    GDEscape "DROMAN",
                    GDEscape "DUNKNOWN"]

-- | Attempt to extract a 'DatePeriod' from a string with escape sequences.
parseDatePeriod :: [(Maybe GDEscape, T.Text)] -> Maybe DatePeriod
parseDatePeriod t = case prepareDateText t of
  ((mCalendarEscape1, t1'):rest) ->
    let
      calendar1 = decodeCalendarEscape mCalendarEscape1
      t1 = trim$ T.toUpper t1'
      from = trim <$> T.stripPrefix "FROM" t1
      to = trim <$> T.stripPrefix "TO" t1
    in case to of
      Just r -> DateTo <$> getDate calendar1 r
      Nothing -> case from of
        Nothing -> Nothing
        Just r ->
          let topcs = T.splitOn "TO" r
          in case topcs of
            [mfdate, mto] -> DateFrom
                <$> getDate calendar1 (trim mfdate)
                <*> pure (getDate calendar1 (trim mto))
            [mfdate] -> case rest of
                [] -> DateFrom
                  <$> getDate calendar1 (trim mfdate) <*> pure Nothing
                ((mCalendarEscape2, t2'):_) ->
                  let
                    calendar2 = decodeCalendarEscape mCalendarEscape2
                    t2 = trim$ T.toUpper t2'
                    to2 = trim <$> T.stripPrefix "TO" t2
                  in case to2 of
                    Nothing -> Nothing
                    Just r2 -> DateFrom
                      <$> getDate calendar1 (trim mfdate)
                      <*> pure (getDate calendar2 (trim r2))
            _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'DateRange' from a string with escape sequences.
parseDateRange :: [(Maybe GDEscape, T.Text)] -> Maybe DateRange
parseDateRange t = case prepareDateText t of
  ((mCalendarEscape1, t1):rest) ->
    let
      calendar1 = decodeCalendarEscape mCalendarEscape1
      (pref, date1) = second trim . T.splitAt 3 . T.toUpper . trim $ t1
    in case pref of
      "BEF" -> DateBefore <$> getDate calendar1 date1
      "AFT" -> DateAfter <$> getDate calendar1 date1
      "BET" -> case T.splitOn "AND" date1 of
        [d1, date2'] -> DateBetween
          <$> getDate calendar1 d1
          <*> getDate calendar1 (trim date2')
        [d1] -> case rest of
          [] -> Nothing
          ((mCalendarEscape2, t2):_) ->
            let calendar2 = decodeCalendarEscape mCalendarEscape2
            in DateBetween
              <$> getDate calendar1 d1
              <*> getDate calendar2 (trim t2)
        _ -> Nothing
      _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'DateApprox' from a string with escape sequences.
parseDateApprox :: [(Maybe GDEscape, T.Text)] -> Maybe DateApprox
parseDateApprox t = case prepareDateText t of
  ((mCalendarEscape1, t1):_) ->
    let
      calendar1 = decodeCalendarEscape mCalendarEscape1
      (pref, date) = second trim . T.splitAt 3 . T.toUpper . trim $ t1
      cons = case pref of
        "ABT" -> Just DateAbout
        "CAL" -> Just DateCalculated
        "EST" -> Just DateEstimated
        _ -> Nothing
    in cons <*> getDate calendar1 date
  _ -> Nothing

-- | Attempt to extract a 'DatePhrase' from a string with escape sequences.
parseDatePhrase :: [(Maybe GDEscape, T.Text)] -> Maybe DateValue
parseDatePhrase t = case prepareDateText t of
  ((mCalendarEscape1, t1):_) ->
    let
      calendar1 = decodeCalendarEscape mCalendarEscape1
      int = trim <$> T.stripPrefix "INT" t1
      opp = trim <$> T.stripPrefix "(" t1
    in case int of
      Nothing -> case opp of
        Nothing -> Nothing
        Just phrase -> Just$
          DatePhrase Nothing (trim . T.dropEnd 1$ phrase)
      Just rest -> case trim <$> T.splitOn "(" rest of
        [date, phrase] -> Just$ 
          DatePhrase (getDate calendar1 date) (trim . T.dropEnd 1$ phrase)
        _ -> Nothing
  _ -> Nothing

-- | Attempt to extract a 'Date' from a string with escape sequences.
parseDate :: [(Maybe GDEscape, T.Text)] -> Maybe Date
parseDate t = case prepareDateText t of
    ((mCalendarEscape, cal):rest) ->
      let calendar = decodeCalendarEscape mCalendarEscape
      in getDate calendar (cal <> gdIgnoreEscapes rest)
    _ -> Nothing

-- | Parse a 'Date' from a given string assuming the given 'Calendar'.
getDate :: Calendar -> T.Text -> Maybe Date
getDate calendar = parseMaybe parser
  where
    yearParser = case calendar of
      Gregorian -> yearGreg
      _ -> read <$> count' 1 4 digitChar
    parseYear = (\y bc -> Year$ if bc then negate y else y)
      <$> yearParser <*> ((True <$ "B.C.") <|> pure False)
    parser :: Parser Date
    parser =
      try (Date calendar
          <$> (Just . read <$> count' 1 2 digitChar)
          <*> (gdDelim >> (Just <$> parseMonth))
          <*> (gdDelim >> parseYear))
      <|> try (Date calendar Nothing
          <$> (gdDelim >> (Just <$> parseMonth))
          <*> (gdDelim >> parseYear))
      <|> (Date calendar Nothing Nothing
          <$> (gdDelim >> parseYear))
    parseMonth = case calendar of
                   Gregorian -> month
                   Julian -> month
                   Hebrew -> monthHeb
                   French -> monthFr
      
-- | Parse an exact date.
parseExactDate :: StructureParser UTCTime
parseExactDate = parseNoLinkTag (GDTag "DATE")$ \(date, _) ->
  case parseMaybe dateExact (gdIgnoreEscapes date) of
    Nothing -> throwError.FormatError$ "Bad date \"" <> T.show date <> "\""
    Just (d, m, y) ->
      return$ UTCTime
        (fromGregorian (fromIntegral y) (fromIntegral m) d)
        (secondsToDiffTime 0)

-- | Parse an exact date and time.
parseExactDateTime :: StructureParser UTCTime
parseExactDateTime = parseNoLinkTag (GDTag "DATE")$ \(date, children) -> do
  mtime <- runMultiMonad children$ parseOptional (parseTextTag (GDTag "TIME"))

  dt <- case mtime of
    Nothing -> return$ secondsToDiffTime 0
    Just t -> case parseMaybe timeValue t of
      Nothing -> throwError.FormatError$ "Bad time \"" <> T.show t <> "\""
      Just Nothing -> return$ secondsToDiffTime 0
      Just (Just time) -> return$ timeToPicos time

  case parseMaybe dateExact (gdIgnoreEscapes date) of
    Nothing -> throwError.FormatError$ "Bad date \"" <> T.show date <> "\""
    Just (d, m, y) -> return$ UTCTime
      (fromGregorian (fromIntegral y) (fromIntegral m) d) dt

-- | Parse a 'Multimedia' record.
parseMultimediaRecord :: StructureParser (GDRef Multimedia)
parseMultimediaRecord = parseMultimediaRaw (GDTag "TYPE")

-- | Parse a 'Multimedia' link.
parseMultimedia :: StructureParser (GDRef Multimedia)
parseMultimedia = parseMultimediaRaw (GDTag "MEDI")

-- | Actual parser for 'Multimedia' values.
parseMultimediaRaw :: GDTag -> StructureParser (GDRef Multimedia)
parseMultimediaRaw typeTag = parseTag (GDTag "OBJE")$ \(_, children) ->
  runMultiMonad children$ Multimedia
    <$> (parseOptional (parseMultimediaFormat typeTag) >>=
      (parseMulti.parseMultimediaFile typeTag))
    <*> parseOptional (parseTextTag (GDTag "TITL"))
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation
    <*> parseOptional parseChangeDate

-- | Parse a 'MultimediaFile'.
parseMultimediaFile ::
  GDTag -> Maybe MultimediaFormat ->
  StructureParser MultimediaFile
parseMultimediaFile typeTag mf = parseNoLinkTag (GDTag "FILE")$
  \(name, children) -> do
    (mc, title) <- runMultiMonad children$ (,)
      <$> parseOptional (parseMultimediaFormat typeTag)
      <*> parseOptional (parseTextTag (GDTag "TITL"))
    case mc <|> mf of
      Nothing -> throwError.TagError$ "Missing FORM tag for file format"
      Just c -> return$ MultimediaFile (gdIgnoreEscapes name) c title

-- | Parse a 'MultimediaFormat'.
parseMultimediaFormat :: GDTag -> StructureParser MultimediaFormat
parseMultimediaFormat tag = parseNoLinkTag (GDTag "FORM")$ \(v', children) ->
  let v = gdIgnoreEscapes v'
  in runMultiMonad children$ MultimediaFormat
    (fromMaybe (MF_OTHER v)$ parseMaybe parser v)
    <$> parseOptional (parseMultimediaType tag)

  where
    parser :: Parser MultimediaFileFormat
    parser =     (MF_BMP <$ "bmp")
             <|> (MF_GIF <$ "gif")
             <|> (MF_JPG <$ "jpg")
             <|> (MF_OLE <$ "ole")
             <|> (MF_PCX <$ "pcx")
             <|> (MF_TIF <$ "tif")
             <|> (MF_WAV <$ "wav")

-- | Parse a 'MultimediaType'.
parseMultimediaType :: GDTag -> StructureParser MultimediaType
parseMultimediaType tag = parseNoLinkTag tag$ \(v', _) ->
  let v = gdIgnoreEscapes v'
  in return.fromMaybe (MT_OTHER v)$ parseMaybe parser v
  where
    parser :: Parser MultimediaType
    parser =     (MT_AUDIO <$ "audio")
             <|> (MT_BOOK <$ "book")
             <|> (MT_CARD <$ "card")
             <|> (MT_ELECTRONIC <$ "electronic")
             <|> (MT_FICHE <$ "fiche")
             <|> (MT_FILM <$ "film")
             <|> (MT_MAGAZINE <$ "magazine")
             <|> (MT_MANUSCRIPT <$ "manuscript")
             <|> (MT_MAP <$ "map")
             <|> (MT_NEWSPAPER <$ "newspaper")
             <|> (MT_PHOTO <$ "photo")
             <|> (MT_TOMBSTONE <$ "tombstone")
             <|> (MT_VIDEO <$ "video")

-- | Parse a 'FamilyEventType'.
getFamilyEventType :: T.Text -> Maybe FamilyEventType
getFamilyEventType = parseMaybe parser
  where
    parser :: Parser FamilyEventType
    parser =     (Annuled <$ "ANUL")
             <|> (FamCensus <$ "CENS")
             <|> (Divorce <$ "DIV")
             <|> (DivorceFiled <$ "DIVF")
             <|> (Engagement <$ "ENGA")
             <|> (MarriageBann <$ "MARB")
             <|> (MarriageContract <$ "MARC")
             <|> (Marriage <$ "MARR")
             <|> (MarriageLicense <$ "MARL")
             <|> (MarriageSettlement <$ "MARS")
             <|> (Residence <$ "RESI")
             <|> (FamilyEventType . T.pack <$>
               ("EVEN" *> gdDelim *> many anyChar))

-- | Parse an 'IndividualEventType'.
getIndividualEventType :: T.Text -> Maybe IndividualEventType
getIndividualEventType = parseMaybe parser
  where
    parser :: Parser IndividualEventType
    parser =     (Birth Nothing <$ ("BIRTH"
                    *> optional (gdDelim >> "Y")))
             <|> (Christening Nothing <$ ("CHR"
                    *> optional (gdDelim >> "Y")))
             <|> (Death <$ ("DEAT"
                    *> optional (gdDelim >> "Y")))
             <|> (Burial <$ "BURI")
             <|> (Cremation <$ "CREM")
             <|> (Adoption Nothing <$ "ADOP")
             <|> (Baptism <$ "BAPM")
             <|> (BarMitzvah <$ "BARM")
             <|> (BasMitzvah <$ "BASM")
             <|> (Blessing <$ "BLES")
             <|> (ChristeningAdult <$ "CHRA")
             <|> (Confirmation <$ "CONF")
             <|> (FirstCommunion <$ "FCOM")
             <|> (Ordination <$ "ORDN")
             <|> (Naturalization <$ "NATU")
             <|> (Emigration <$ "EMIG")
             <|> (Immigration <$ "IMMI")
             <|> (IndvCensus <$ "CENS")
             <|> (Probate <$ "PROB")
             <|> (Will <$ "WILL")
             <|> (Graduation <$ "GRAD")
             <|> (Retirement <$ "RETI")
             <|> (IndividualEventType . T.pack <$>
               ("EVEN" *> gdDelim *> many anyChar))

-- | Parse an 'EventType'.
getEventType :: T.Text -> Maybe EventType
getEventType et =
  let
    t = trim$ T.toUpper et
    fam = getFamilyEventType et
    indv = getIndividualEventType et
  in
    if t == "CENS" then Just Census
    else if T.take 4 t == "EVEN" then
      Just . EventType . trim . T.drop 4$ et
    else fmap FamilyEventTypeV fam <|> fmap IndividualEventTypeV indv

-- | Parse a place form structure.
parsePlaceForm :: StructureParser [T.Text]
parsePlaceForm = parseNoLinkTag (GDTag "PLAC")$ \(_, children) ->
  runMultiMonad children$ maybe [] (T.splitOn ",") <$>
    parseOptional (parseTextTag (GDTag "FORM"))

-- | Parse a 'UserReference'.
parseUserReference :: StructureParser UserReference
parseUserReference = parseNoLinkTag (GDTag "REFN")$ \(i, children) ->
  runMultiMonad children$ UserReference (gdIgnoreEscapes i)
    <$> parseOptional (parseTextTag (GDTag "TYPE"))

-- | Parse a version structure.
parseVersion :: StructureParser T.Text
parseVersion = parseTextTag (GDTag "VERS")

-- | Parse a NAME tag.
parseName :: StructureParser T.Text
parseName = parseTextTag (GDTag "NAME")

-- | Parse a copyright tag.
parseCopyright :: StructureParser T.Text
parseCopyright = parseTextTag (GDTag "COPR")

-- | Parse a file name.
parseFile :: StructureParser FilePath
parseFile = (fmap.fmap) T.unpack <$> parseTextTag (GDTag "FILE")

-- | Parse an 'AFN'.
parseAFN :: StructureParser AFN
parseAFN = (fmap.fmap) AFN <$> parseTextTag (GDTag "AFN")

-- | Parse a 'RFN'.
parseRFN :: StructureParser RFN
parseRFN = (fmap.fmap) RFN <$> parseTextTag (GDTag "RFN")

-- | Parse a 'RIN'.
parseRIN :: StructureParser RIN
parseRIN = (fmap.fmap) RIN <$> parseTextTag (GDTag "RIN")

-- | Parse a 'Language'.
parseLanguage :: StructureParser Language
parseLanguage = (fmap.fmap) Language <$> parseTextTag (GDTag "LANG")

-- | Parse a 'QualityAssessment'.
parseQuality :: StructureParser QualityAssessment
parseQuality = (fmap.fmap) QualityAssessment <$> parseWordTag (GDTag "QUAY")

-- | Parse a boolean value.
parseBoolTag :: GDTag -> StructureParser Bool
parseBoolTag tag = parseNoLinkTag tag$ \(v, _) ->
  case parseMaybe ynParser (gdIgnoreEscapes v) of
    Nothing -> throwError.FormatError$ "Expected boolean, saw " <> T.show v
    Just yn -> return yn
  where
    ynParser :: Parser Bool
    ynParser = (True <$ string' "yes") <|> (False <$ string' "no")

-- | Parse a Word value.
parseWordTag :: GDTag -> StructureParser Word
parseWordTag tag = parseNoLinkTag tag$ \(v, _) ->
  case parseMaybe parser (gdIgnoreEscapes v) of
    Nothing -> throwError.FormatError$ "Expected number, saw " <> T.show v
    Just n -> return . read $ n
  where
    parser :: Parser String
    parser = many digitChar

-- | Extract the text from a tag.
parseTextTag :: GDTag -> StructureParser T.Text
parseTextTag tag = parseNoLinkTag tag (return.gdIgnoreEscapes.fst)

-- | Extract a list of comma separated values from a tag.
parseListTag :: GDTag -> StructureParser [T.Text]
parseListTag tag =
  parseNoLinkTag tag (return . split . trim . gdIgnoreEscapes . fst)
  where
    split "" = []
    split s = T.splitOn "," s

-- | Handler for tags that cannot contain cross references.
type NoLinkHandler a =
  ([(Maybe GDEscape, T.Text)], [GDTree]) -> StructureMonad a

-- | Handler for general tags.
type TagHandler b a =
  (Either (GDRef b) [(Maybe GDEscape, T.Text)], [GDTree])
  -> StructureMonad a

-- | A function that adds references to the cross reference table.
type RegisterRef a = GDTag -> GDXRefID -> a -> StructureMonad ()

-- | Don't add any references.
nullrref :: Typeable a => RegisterRef a
nullrref _ _ _ = return ()

-- | Add direct references only.
defrref :: Typeable a => RegisterRef (GDRef a)
defrref _ thisID (GDStructure a) = addReference thisID a
defrref tag _ _ = throwError.UnexpectedRef$
  "Referenced structure references another structure " <> T.show tag

-- | Parse a tag which is either a GEDCOM structure, or a reference to the
-- expected GEDCOM structure.
parseTag :: Typeable a
  => GDTag            -- ^ The tag to parse.
  -> NoLinkHandler a  -- ^ A handler for the tag
  -> StructureParser (GDRef a)
parseTag tag handler = parseTagFull tag defrref$ \(lb, children) ->
  case lb of
    Left ref -> return ref
    Right text -> GDStructure <$> handler (text, children)

-- | Parse a tag which cannot contain a cross reference (i.e. the tag must
-- contain the structure itself, not a reference to another structure).
parseNoLinkTag :: Typeable a
  => GDTag            -- ^ The tag to parse.
  -> NoLinkHandler a  -- ^ A handler for the tag.
  -> StructureParser a
parseNoLinkTag tag handler = parseTagFull tag nullrref$ \(lb, children) ->
  case lb of
    Left _ -> throwError.UnexpectedRef$
      "Cannot follow cross references on " <> T.show tag
    Right text -> handler (text, children)

-- | Parse a tag which must contain a cross reference to another structure, not
-- the structure itself.
parseLinkTag :: Typeable a => GDTag -> StructureParser (GDRef a)
parseLinkTag tag = parseTagFull tag nullrref$ \(lb, _) ->
  case lb of
    Left ref -> return ref
    Right _ -> throwError.RequiredRef$
      "Expected cross reference was missing in " <> T.show tag

-- | The most general tag parsing function.
parseTagFull :: Typeable a
  => GDTag             -- ^ The tag to parse.
  -> RegisterRef a     -- ^ How to register cross references.
  -> TagHandler b a    -- ^ How to convert the tag into a GEDCOM structure.
  -> StructureParser a
parseTagFull tag rref handler t@(GDTree (GDLine _ mthisID tag' v) children) =
  if tag /= tag' then return$ Left t else do
    r <- case v of
      Nothing -> Right <$> (handler.first Right$ parseCont mempty children)
      Just (GDXRefIDV xref) ->
        Right <$> handler (Left (GDXRef xref), children)
      Just (GDLineItemV l1) ->
        Right <$> (handler.first Right$ parseCont l1 children)
    case (mthisID, r) of
      (Just thisID, Right rv) -> rref tag thisID rv
      _ -> return ()
    return r

-- | Handle CONT and CONC tags.
parseCont ::
     GDLineItem -- ^ The value of the first line
  -> [GDTree]   -- ^ The sub tree
  -> ([(Maybe GDEscape, T.Text)], [GDTree])  -- ^ The concatenation of the values of all the CONT and CONC tags, and the remaining tags.
parseCont l1 children =
    bimap assemble (fmap snd).span (isJust.fst)$
      zipWith (\l r -> (cont l <|> conc l, r)) children children
  where
    assemble = gdLineData . mconcat . (l1:) . catMaybes . fmap fst
    cont (GDTree (GDLine _ _ (GDTag "CONT") (Just (GDLineItemV l))) _) =
      Just$ GDLineItem [(Nothing, "\r\n")] <> l
    cont _ = Nothing
    conc (GDTree (GDLine _ _ (GDTag "CONC") (Just (GDLineItemV l))) _) =
      Just$ gdTrimLineItem l
    conc _ = Nothing

