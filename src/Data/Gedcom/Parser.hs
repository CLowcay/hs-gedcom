{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Data.Gedcom.Parser where

import Control.Applicative
import Control.Monad.Except
import Data.Bifunctor
import Data.Dynamic
import Data.Gedcom.Common
import Data.Gedcom.Internal.Common
import Data.Gedcom.LineParser
import Data.Gedcom.ParseMonads
import Data.Gedcom.Structure
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Map.Lazy as M
import qualified Data.Text.All as T
import Text.Megaparsec

-- The root structure

parseGedcom :: GDRoot -> (Either GDError Gedcom, M.Map GDXRefID Dynamic)
parseGedcom (GDRoot children) =
  runStructure.runMultiMonad children$ Gedcom
    <$> parseRequired (GDTag "HEAD") parseHeader
    <*> parseMulti parseFamily
    <*> parseMulti (parseIndividual (GDTag "INDI"))
    <*> parseMulti parseMultimedia
    <*> parseMulti parseNote
    <*> parseMulti parseRepository
    <*> parseMulti parseSource
    <*> parseMulti (parseSubmitter (GDTag "SUBM"))

-- Header
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

parseHeaderSource :: StructureParser HeaderSource
parseHeaderSource = parseNoLinkTag (GDTag "SOUR")$ \(sid, children) ->
  runMultiMonad children$ HeaderSource (gdIgnoreEscapes sid)
    <$> parseOptional parseVersion
    <*> parseOptional parseName
    <*> parseOptional parseCorp
    <*> parseOptional parseHeaderSourceData

parseCorp :: StructureParser Corp
parseCorp = parseNoLinkTag (GDTag "COPR")$ \(name, children) ->
  runMultiMonad children$ Corp (gdIgnoreEscapes name)
    <$> (parseContactDetails >>= (parseOptional.parseAddress))

parseHeaderSourceData :: StructureParser HeaderSourceData
parseHeaderSourceData = parseNoLinkTag (GDTag "DATA")$ \(name, children) ->
  runMultiMonad children$ HeaderSourceData (gdIgnoreEscapes name)
    <$> parseOptional parseExactDate
    <*> parseOptional parseCopyright

-- Records

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

parseNote :: StructureParser (GDRef Note)
parseNote = parseTag (GDTag "NOTE")$ \(t, children) ->
  runMultiMonad children$ Note (gdIgnoreEscapes t)
    <$> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseMulti parseSourceCitation
    <*> parseOptional parseChangeDate

parseRepository :: StructureParser (GDRef Repository)
parseRepository = parseTag (GDTag "REPO")$ \(_, children) ->
  runMultiMonad children$ Repository
    <$> parseRequired (GDTag "NAME") parseName
    <*> (parseContactDetails >>= (parseOptional.parseAddress))
    <*> parseMulti parseNote
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseOptional parseChangeDate

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

parseSubmitter :: GDTag -> StructureParser (GDRef Submitter)
parseSubmitter tag = parseTag tag$ \(_, children) ->
  runMultiMonad children$ Submitter
    <$> parseRequired (GDTag "NAME") parseName
    <*> (parseContactDetails >>= (parseOptional.parseAddress))
    <*> parseOptional parseMultimedia
    <*> parseMulti parseLanguage
    <*> parseOptional parseRFN
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseOptional parseChangeDate

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

-- Substructures

parseRestrictionNotice :: StructureParser RestrictionNotice
parseRestrictionNotice = parseNoLinkTag (GDTag "RESN")$ \(t, _) ->
  case parseMaybe parser (gdIgnoreEscapes t) of
    Nothing -> throwError.FormatError$
      "Bad restriction notice " <> (T.show t)
    Just r -> return r
  where parser :: Parser RestrictionNotice
        parser =     (Confidential <$ string' "confidential")
                 <|> (Locked <$ string' "locked")
                 <|> (Privacy <$ string' "privacy")

parseFamilyEvent :: MultiMonad [FamilyEvent]
parseFamilyEvent = do
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

parseFamilyEventDetail :: MultiMonad FamilyEventDetail
parseFamilyEventDetail = FamilyEventDetail
  <$> parseOptional (parseAge (GDTag "HUSB"))
  <*> parseOptional (parseAge (GDTag "WIFE"))
  <*> parseEventDetail

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

parseAge :: GDTag -> StructureParser Word
parseAge tag = parseNoLinkTag tag$ \(_, children) ->
  runMultiMonad children$
    parseRequired (GDTag "AGE") (parseWordTag (GDTag "AGE"))

parseSourceData :: StructureParser SourceData
parseSourceData = parseNoLinkTag (GDTag "DATA")$ \(_, children) ->
  runMultiMonad children$ SourceData
    <$> parseMulti parseSourceRecordedEvent
    <*> parseOptional (parseTextTag (GDTag "AGNC"))
    <*> parseMulti parseNote

parsePlace :: StructureParser Place
parsePlace = parseNoLinkTag (GDTag "PLAC")$ \(t, children) ->
  runMultiMonad children$ Place
    <$> (pure$ T.splitOn "," . gdIgnoreEscapes$ t)
    <*> parseOptional (parseListTag (GDTag "FORM"))
    <*> parseOptional parsePhoneticPlaceName
    <*> parseOptional parseRomanPlaceName
    <*> parseOptional parseMapCoord
    <*> parseMulti parseNote

parsePhoneticPlaceName :: StructureParser PhoneticPlaceName
parsePhoneticPlaceName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ PhoneticPlaceName
    <$> parseRequired (GDTag "TYPE") parsePhoneticType
    <*> (pure . T.splitOn "," . gdIgnoreEscapes$ t)

parseRomanPlaceName :: StructureParser RomanPlaceName
parseRomanPlaceName = parseNoLinkTag (GDTag "ROMN")$ \(t, children) ->
  runMultiMonad children$ RomanPlaceName
    <$> parseRequired (GDTag "TYPE") parseRomanType
    <*> (pure . T.splitOn "," . gdIgnoreEscapes$ t)

parsePhoneticType :: StructureParser PhoneticType
parsePhoneticType = parseNoLinkTag (GDTag "TYPE")$ \(t, _) ->
  return$ case trim . T.toUpper . gdIgnoreEscapes$ t of
    "HANGUL" -> Hangul
    "KANA" -> Kana
    v -> PhoneticType v

parseRomanType :: StructureParser RomanType
parseRomanType = parseNoLinkTag (GDTag "TYPE")$ \(t, _) ->
  return$ case trim . T.toUpper . gdIgnoreEscapes$ t of
    "PINYIN" -> Pinyin
    "ROMAJI" -> Romaji
    "WADEGILES" -> WadeGiles
    v -> RomanType v

parseMapCoord :: StructureParser MapCoord
parseMapCoord = parseNoLinkTag (GDTag "MAP")$ \(_, children) ->
  runMultiMonad children$ MapCoord
    <$> parseRequired (GDTag "LATI") ((fmap.fmap.fmap) Longitude$
      parseLongLat (GDTag "LATI") 'N' 'S')
    <*> parseRequired (GDTag "LONG") ((fmap.fmap.fmap) Latitude$
      parseLongLat (GDTag "LONG") 'E' 'W')

parseLongLat :: GDTag -> Char -> Char -> StructureParser Double
parseLongLat tag p n = parseNoLinkTag tag$ \(t, _) ->
  case T.uncons . T.toUpper . gdIgnoreEscapes$ t of
    Nothing -> throwError.FormatError$
      "Badly formatted longitude/latitude " <> (T.show t)
    Just (i, r) ->
      if i == p then return$ read . T.unpack$ r
      else if i == n then return$ negate . read . T.unpack$ r
      else throwError.FormatError$
        "Badly formatted longitude/latitude" <> (T.show t)

parsePersonalName :: StructureParser PersonalName
parsePersonalName = parseNoLinkTag (GDTag "NAME")$ \(n, children) ->
  runMultiMonad children$ PersonalName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ n)
    <*> parseOptional parseNameType
    <*> parseNamePieces
    <*> parseMulti parsePhoneticName
    <*> parseMulti parseRomanName
    
getPersonalName :: T.Text -> Name
getPersonalName = Name <$> (T.filter (/= '/')) <*> (parseMaybe parser)
  where parser :: Parser T.Text
        parser = (many$ noneOf ['/']) *> (T.pack <$> many (noneOf ['/']))

parseNameType :: StructureParser NameType
parseNameType = parseNoLinkTag (GDTag "TYPE")$ \(t', _) ->
  let t = gdIgnoreEscapes t'
  in return . fromMaybe (NameType t) . parseMaybe parser $ t
  where parser :: Parser NameType
        parser =     (AKA <$ string' "aka")
                 <|> (BirthName <$ string' "birth")
                 <|> (Immigrant <$ string' "immigrant")
                 <|> (Maiden <$ string' "maiden")
                 <|> (Married <$ string' "married")

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

parsePhoneticName :: StructureParser PhoneticName
parsePhoneticName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ PhoneticName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ t)
    <*> parseRequired (GDTag "TYPE") parsePhoneticType
    <*> parseNamePieces

parseRomanName :: StructureParser RomanizedName
parseRomanName = parseNoLinkTag (GDTag "FONE")$ \(t, children) ->
  runMultiMonad children$ RomanizedName
    <$> (pure.getPersonalName.gdIgnoreEscapes$ t)
    <*> parseRequired (GDTag "TYPE") parseRomanType
    <*> parseNamePieces

parseSex :: StructureParser Sex
parseSex = parseNoLinkTag (GDTag "SEX")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes $ t of
    "M" -> return Male
    "F" -> return Female
    "U" -> return Undetermined
    _ -> throwError.FormatError$ "Unknown sex code " <> (T.show t)
  
parseIndividualAttribute :: MultiMonad [IndividualAttribute]
parseIndividualAttribute = do
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
    parseAdopFamilyRef = parseTagFull (GDTag "FAMC")$ \(lb, children) ->
      case lb of
        Right _ -> throwError.RequiredRef$ "Missing link in FAMC"
        Left f -> runMultiMonad children$
          AdoptionDetail f <$> parseOptional parseParent
    parseParent = parseNoLinkTag (GDTag "ADOP")$ \(t, _) ->
      case trim . T.toUpper . gdIgnoreEscapes $ t of
        "HUSB" -> return Husband
        "WIFE" -> return Wife
        "BOTH" -> return BothParents
        _ -> throwError.FormatError$ "Invalid parent " <> (T.show t)

parseIndividualEventDetail :: MultiMonad IndividualEventDetail
parseIndividualEventDetail = IndividualEventDetail
  <$> parseEventDetail
  <*> parseOptional (parseWordTag (GDTag "AGE"))

parseChildToFamilyLink :: StructureParser ChildToFamilyLink
parseChildToFamilyLink = parseTagFull (GDTag "FAMC")$ \(lb, children) ->
  case lb of
    Right _ -> throwError.RequiredRef$ "Missing link in FAMC"
    Left f -> runMultiMonad children$ ChildToFamilyLink f
      <$> parseOptional parsePedigree
      <*> parseOptional parseChildLinkStatus
      <*> parseMulti parseNote

parsePedigree :: StructureParser Pedigree
parsePedigree = parseNoLinkTag (GDTag "PEDI")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes$ t of
    "ADOPTED" -> return Adopted
    "BIRTH" -> return ByBirth
    "FOSTER" -> return Foster
    "SEALING" -> return Sealing
    _ -> throwError.FormatError$ "Invalid pedigree code " <> (T.show t)

parseChildLinkStatus :: StructureParser ChildLinkStatus
parseChildLinkStatus = parseNoLinkTag (GDTag "STAT")$ \(t, _) ->
  case trim . T.toUpper . gdIgnoreEscapes$ t of
    "CHALLENGED" -> return Challenged
    "DISPROVEN" -> return Disproved
    "PROVEN" -> return Proven
    _ -> throwError.FormatError$ "Invalid child link status " <> (T.show t)

parseSpouseToFamilyLink :: StructureParser SpouseToFamilyLink
parseSpouseToFamilyLink = parseTagFull (GDTag "FAMS")$ \(lb, children) ->
  case lb of
    Right _ -> throwError.RequiredRef$ "Missing link in FAMS"
    Left f -> runMultiMonad children$ SpouseToFamilyLink f
      <$> parseMulti parseNote

parseAssociation :: StructureParser Association
parseAssociation = parseTagFull (GDTag "ASSO")$ \(lb, children) ->
  case lb of
    Right _ -> throwError.RequiredRef$ "Missing link in ASSO"
    Left i -> runMultiMonad children$ Association i
      <$> parseRequired (GDTag "RELA") (parseTextTag (GDTag "RELA"))
      <*> parseMulti parseSourceCitation
      <*> parseMulti parseNote

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
            "Badly formatted date period: " <> (T.show date)

parseRepositoryCitation :: StructureParser RepositoryCitation
parseRepositoryCitation = parseTagFull (GDTag "REPO")$ \(lb, children) ->
  let repo = case lb of
               Left v -> Just v
               Right _ -> Nothing
  in runMultiMonad children$ RepositoryCitation repo
    <$> parseMulti parseNote
    <*> parseOptional parseCallNumber

parseCallNumber :: StructureParser CallNumber
parseCallNumber = parseNoLinkTag (GDTag "CALN")$ \(n, children) ->
  runMultiMonad children$ CallNumber (gdIgnoreEscapes n)
    <$> parseOptional (parseMultimediaType (GDTag "MEDI"))

parseSourceCitation :: StructureParser SourceCitation
parseSourceCitation = parseTagFull (GDTag "SOUR")$ \(lb, children) ->
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

parseGedcomFormat :: StructureParser GedcomFormat
parseGedcomFormat = parseNoLinkTag (GDTag "GEDC")$ \(_, children) ->
  runMultiMonad children$ GedcomFormat
    <$> parseRequired (GDTag "VERS") parseVersion
    <*> parseRequired (GDTag "FORM") parseGedcomForm

parseGedcomForm :: StructureParser GedcomForm
parseGedcomForm = parseNoLinkTag (GDTag "FORM")$ \(t, _) ->
  return$ if (T.toUpper . gdIgnoreEscapes$ t) == "LINEAGE-LINKED"
    then GedcomLineageLinked
    else GedcomUnsupported (gdIgnoreEscapes t)

parseCharset :: StructureParser Charset
parseCharset = parseNoLinkTag (GDTag "CHAR")$ \(cs, children) ->
  runMultiMonad children$ Charset (gdIgnoreEscapes cs)
    <$> parseOptional parseVersion

parseChangeDate :: StructureParser ChangeDate
parseChangeDate = parseNoLinkTag (GDTag "CHAN")$ \(_, children) ->
  runMultiMonad children$ ChangeDate
    <$> parseRequired (GDTag "DATE") parseExactDateTime
    <*> parseOptional parseNote

parseContactDetails :: MultiMonad ContactDetails
parseContactDetails = ContactDetails
  <$> parseMulti (parseTextTag (GDTag "PHON"))
  <*> parseMulti (parseTextTag (GDTag "EMAIL"))
  <*> parseMulti (parseTextTag (GDTag "FAX"))
  <*> parseMulti (parseTextTag (GDTag "WWW"))

parseAddress :: ContactDetails -> StructureParser Address
parseAddress contacts = parseNoLinkTag (GDTag "ADDR")$ \(addr, children) ->
  runMultiMonad children$ Address (gdIgnoreEscapes addr)
    <$> parseOptional (parseTextTag (GDTag "CITY"))
    <*> parseOptional (parseTextTag (GDTag "STAE"))
    <*> parseOptional (parseTextTag (GDTag "POS"))
    <*> parseOptional (parseTextTag (GDTag "CTRY"))
    <*> pure contacts

-- Values

parseDateValue :: StructureParser DateValue
parseDateValue = parseNoLinkTag (GDTag "DATE")$ \(t, _) ->
  let date = (DateApproxV <$> parseDateApprox t)
         <|> (DateRangeV <$> parseDateRange t)
         <|> (DatePeriodV <$> parseDatePeriod t)
         <|> (parseDatePhrase t)
         <|> (DateV <$> parseDate t)
  in case date of
    Just x -> return x
    Nothing -> throwError.FormatError$ "Invalid date format " <> (T.show t)

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

prepareDateText :: [(Maybe GDEscape, T.Text)] -> [(Maybe GDEscape, T.Text)]
prepareDateText = gdFilterEscapes [
                    GDEscape "DGREGORIAN",
                    GDEscape "DJULIAN",
                    GDEscape "DHEBREW",
                    GDEscape "DFRENCH",
                    GDEscape "DROMAN",
                    GDEscape "DUNKNOWN"]

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
          let topcs = T.splitOn "TO"$ r
          in case topcs of
            [mfdate, mto] -> DateFrom
                <$> getDate calendar1 (trim mfdate)
                <*> (pure$ getDate calendar1 (trim mto))
            [mfdate] -> case rest of
                [] -> DateFrom
                  <$> getDate calendar1 (trim mfdate) <*> (pure Nothing)
                ((mCalendarEscape2, t2'):_) ->
                  let
                    calendar2 = decodeCalendarEscape mCalendarEscape2
                    t2 = trim$ T.toUpper t2'
                    to2 = trim <$> T.stripPrefix "TO" t2
                  in case to2 of
                    Nothing -> Nothing
                    Just r2 -> DateFrom
                      <$> getDate calendar1 (trim mfdate)
                      <*> (pure$ getDate calendar2 (trim r2))
            _ -> Nothing
  _ -> Nothing

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

parseDate :: [(Maybe GDEscape, T.Text)] -> Maybe Date
parseDate t = case prepareDateText t of
    ((mCalendarEscape, cal):rest) ->
      let calendar = decodeCalendarEscape mCalendarEscape
      in getDate calendar (cal <> gdIgnoreEscapes rest)
    _ -> Nothing

getDate :: Calendar -> T.Text -> Maybe Date
getDate calendar = parseMaybe parser
  where
    yearParser = case calendar of
      Gregorian -> yearGreg
      _ -> read <$> count' 1 4 digitChar
    parseYear = (\y bc -> Year$ if bc then (0 - y) else y)
      <$> yearParser <*> ((True <$ string' "B.C.") <|> pure False)
    parser :: Parser Date
    parser =
      (try$ Date calendar
          <$> (Just . read <$> count' 1 2 digitChar)
          <*> (gdDelim >> (Just <$> parseMonth))
          <*> (gdDelim >> parseYear))
      <|> (try$ Date calendar Nothing
          <$> (gdDelim >> (Just <$> parseMonth))
          <*> (gdDelim >> parseYear))
      <|> (Date calendar Nothing Nothing
          <$> (gdDelim >> parseYear))
    parseMonth = case calendar of
                   Gregorian -> month
                   Julian -> month
                   Hebrew -> monthHeb
                   French -> monthFr
      
parseExactDate :: StructureParser UTCTime
parseExactDate = parseNoLinkTag (GDTag "DATE")$ \(date, _) ->
  case parseMaybe dateExact (gdIgnoreEscapes date) of
    Nothing -> throwError.FormatError$ "Bad date \"" <> (T.show date) <> "\""
    Just (d, m, y) ->
      return$ UTCTime
        (fromGregorian (fromIntegral y) (fromIntegral m) d)
        (secondsToDiffTime 0)

parseExactDateTime :: StructureParser UTCTime
parseExactDateTime = parseNoLinkTag (GDTag "DATE")$ \(date, children) -> do
  mtime <- runMultiMonad children$ parseOptional (parseTextTag (GDTag "TIME"))

  dt <- case mtime of
    Nothing -> return$ secondsToDiffTime 0
    Just t -> case parseMaybe timeValue t of
      Nothing -> throwError.FormatError$ "Bad time \"" <> (T.show t) <> "\""
      Just Nothing -> return$ secondsToDiffTime 0
      Just (Just time) -> return$ timeToPicos time

  case parseMaybe dateExact (gdIgnoreEscapes date) of
    Nothing -> throwError.FormatError$ "Bad date \"" <> (T.show date) <> "\""
    Just (d, m, y) -> return$ UTCTime
      (fromGregorian (fromIntegral y) (fromIntegral m) d) dt

parseMultimediaRecord :: StructureParser (GDRef Multimedia)
parseMultimediaRecord = parseMultimediaRaw (GDTag "TYPE")

parseMultimedia :: StructureParser (GDRef Multimedia)
parseMultimedia = parseMultimediaRaw (GDTag "MEDI")

parseMultimediaRaw :: GDTag -> StructureParser (GDRef Multimedia)
parseMultimediaRaw typeTag = parseTag (GDTag "OBJE")$ \(_, children) ->
  runMultiMonad children$ Multimedia
    <$> ((parseOptional (parseMultimediaFormat typeTag)) >>=
      (parseMulti.(parseMultimediaFile typeTag)))
    <*> parseOptional (parseTextTag (GDTag "TITL"))
    <*> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseMulti parseSourceCitation
    <*> parseOptional parseChangeDate

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

parseMultimediaFormat :: GDTag -> StructureParser MultimediaFormat
parseMultimediaFormat tag = parseNoLinkTag (GDTag "FORM")$ \(v', children) ->
  let v = gdIgnoreEscapes v'
  in runMultiMonad children$ MultimediaFormat
    (fromMaybe (MF_OTHER v)$ parseMaybe parser v)
    <$> parseOptional (parseMultimediaType tag)

  where
    parser :: Parser MultimediaFileFormat
    parser =     (MF_BMP <$ string' "bmp")
             <|> (MF_GIF <$ string' "gif")
             <|> (MF_JPG <$ string' "jpg")
             <|> (MF_OLE <$ string' "ole")
             <|> (MF_PCX <$ string' "pcx")
             <|> (MF_TIF <$ string' "tif")
             <|> (MF_WAV <$ string' "wav")

parseMultimediaType :: GDTag -> StructureParser MultimediaType
parseMultimediaType tag = parseNoLinkTag tag$ \(v', _) ->
  let v = gdIgnoreEscapes v'
  in return.fromMaybe (MT_OTHER v)$ parseMaybe parser v
  where
    parser :: Parser MultimediaType
    parser =     (MT_AUDIO <$ string' "audio")
             <|> (MT_BOOK <$ string' "book")
             <|> (MT_CARD <$ string' "card")
             <|> (MT_ELECTRONIC <$ string' "electronic")
             <|> (MT_FICHE <$ string' "fiche")
             <|> (MT_FILM <$ string' "film")
             <|> (MT_MAGAZINE <$ string' "magazine")
             <|> (MT_MANUSCRIPT <$ string' "manuscript")
             <|> (MT_MAP <$ string' "map")
             <|> (MT_NEWSPAPER <$ string' "newspaper")
             <|> (MT_PHOTO <$ string' "photo")
             <|> (MT_TOMBSTONE <$ string' "tombstone")
             <|> (MT_VIDEO <$ string' "video")

getFamilyEventType :: T.Text -> Maybe FamilyEventType
getFamilyEventType = parseMaybe parser
  where
    parser :: Parser FamilyEventType
    parser =     (Annuled <$ string' "ANUL")
             <|> (FamCensus <$ string' "CENS")
             <|> (Divorce <$ string' "DIV")
             <|> (DivorceFiled <$ string' "DIVF")
             <|> (Engagement <$ string' "ENGA")
             <|> (MarriageBann <$ string' "MARB")
             <|> (MarriageContract <$ string' "MARC")
             <|> (Marriage <$ string' "MARR")
             <|> (MarriageLicense <$ string' "MARL")
             <|> (MarriageSettlement <$ string' "MARS")
             <|> (Residence <$ string' "RESI")
             <|> (FamilyEventType . T.pack <$>
               (string' "EVEN" *> gdDelim *> many anyChar))

getIndividualEventType :: T.Text -> Maybe IndividualEventType
getIndividualEventType = parseMaybe parser
  where
    parser :: Parser IndividualEventType
    parser =     (Birth Nothing <$ (string' "BIRTH"
                    *> optional (gdDelim >> string' "Y")))
             <|> (Christening Nothing <$ (string' "CHR"
                    *> optional (gdDelim >> string' "Y")))
             <|> (Death <$ (string' "DEAT"
                    *> optional (gdDelim >> string' "Y")))
             <|> (Burial <$ string' "BURI")
             <|> (Cremation <$ string' "CREM")
             <|> (Adoption Nothing <$ string' "ADOP")
             <|> (Baptism <$ string' "BAPM")
             <|> (BarMitzvah <$ string' "BARM")
             <|> (BasMitzvah <$ string' "BASM")
             <|> (Blessing <$ string' "BLES")
             <|> (ChristeningAdult <$ string' "CHRA")
             <|> (Confirmation <$ string' "CONF")
             <|> (FirstCommunion <$ string' "FCOM")
             <|> (Ordination <$ string' "ORDN")
             <|> (Naturalization <$ string' "NATU")
             <|> (Emigration <$ string' "EMIG")
             <|> (Immigration <$ string' "IMMI")
             <|> (IndvCensus <$ string' "CENS")
             <|> (Probate <$ string' "PROB")
             <|> (Will <$ string' "WILL")
             <|> (Graduation <$ string' "GRAD")
             <|> (Retirement <$ string' "RETI")
             <|> (IndividualEventType . T.pack <$>
               (string' "EVEN" *> gdDelim *> many anyChar))

getEventType :: T.Text -> Maybe EventType
getEventType et =
  let
    t = trim$ T.toUpper et
    fam = getFamilyEventType et
    indv = getIndividualEventType et
  in
    if t == "CENS" then Just Census
    else if (T.take 4 t) == "EVEN" then
      Just . EventType . trim . T.drop 4$ et
    else fmap FamilyEventTypeV fam <|> fmap IndividualEventTypeV indv

parsePlaceForm :: StructureParser [T.Text]
parsePlaceForm = parseNoLinkTag (GDTag "PLAC")$ \(_, children) ->
  runMultiMonad children$ fromMaybe [].(fmap$ T.splitOn ",") <$>
    parseOptional (parseTextTag (GDTag "FORM"))

parseUserReference :: StructureParser UserReference
parseUserReference = parseNoLinkTag (GDTag "REFN")$ \(i, children) ->
  runMultiMonad children$ UserReference (gdIgnoreEscapes i)
    <$> parseOptional (parseTextTag (GDTag "TYPE"))

parseVersion :: StructureParser T.Text
parseVersion = parseTextTag (GDTag "VERS")

parseName :: StructureParser T.Text
parseName = parseTextTag (GDTag "NAME")

parseCopyright :: StructureParser T.Text
parseCopyright = parseTextTag (GDTag "COPR")

parseFile :: StructureParser FilePath
parseFile = (fmap.fmap.fmap) T.unpack$ parseTextTag (GDTag "FILE")

parseAFN :: StructureParser AFN
parseAFN = (fmap.fmap.fmap) AFN$ parseTextTag (GDTag "AFN")

parseRFN :: StructureParser RFN
parseRFN = (fmap.fmap.fmap) RFN$ parseTextTag (GDTag "RFN")

parseRIN :: StructureParser RIN
parseRIN = (fmap.fmap.fmap) RIN$ parseTextTag (GDTag "RIN")

parseLanguage :: StructureParser Language
parseLanguage = (fmap.fmap.fmap) (Language) $ parseTextTag (GDTag "LANG")

parseQuality :: StructureParser QualityAssessment
parseQuality = (fmap.fmap.fmap) QualityAssessment $ parseWordTag (GDTag "QUAY")

-- General parsers

parseBoolTag :: GDTag -> StructureParser Bool
parseBoolTag tag = parseNoLinkTag tag$ \(v, _) ->
  case parseMaybe ynParser (gdIgnoreEscapes v) of
    Nothing -> throwError.FormatError$ "Expected boolean, saw " <> (T.show v)
    Just yn -> return yn
  where
    ynParser :: Parser Bool
    ynParser = (True <$ string' "yes") <|> (False <$ string' "no")

parseWordTag :: GDTag -> StructureParser Word
parseWordTag tag = parseNoLinkTag tag$ \(v, _) ->
  case parseMaybe parser (gdIgnoreEscapes v) of
    Nothing -> throwError.FormatError$ "Expected number, saw " <> (T.show v)
    Just n -> return . read $ n
  where
    parser :: Parser String
    parser = many digitChar

parseTextTag :: GDTag -> StructureParser T.Text
parseTextTag tag = parseNoLinkTag tag (return.gdIgnoreEscapes.fst)

parseListTag :: GDTag -> StructureParser [T.Text]
parseListTag tag =
  parseNoLinkTag tag (return . T.splitOn "," . gdIgnoreEscapes . fst)

type NoLinkHandler a =
  ([(Maybe GDEscape, T.Text)], [GDTree]) -> StructureMonad a

type TagHandler b a =
  (Either (GDRef b) [(Maybe GDEscape, T.Text)], [GDTree])
  -> StructureMonad a

parseTag :: Typeable a => GDTag -> NoLinkHandler a -> StructureParser (GDRef a)
parseTag tag handler = parseTagFull tag$ \(lb, children) ->
  case lb of
    Left ref -> return ref
    Right text -> GDStructure <$> handler (text, children)

parseNoLinkTag :: Typeable a => GDTag -> NoLinkHandler a -> StructureParser a
parseNoLinkTag tag handler = parseTagFull tag$ \(lb, children) ->
  case lb of
    Left _ -> throwError.UnexpectedRef$
      "Cannot follow cross references on " <> (T.show tag)
    Right text -> handler (text, children)

parseLinkTag :: Typeable a => GDTag -> StructureParser (GDRef a)
parseLinkTag tag = parseTagFull tag$ \(lb, _) ->
  case lb of
    Left ref -> return ref
    Right _ -> throwError.RequiredRef$
      "Expected cross reference was missing in " <> (T.show tag)

parseTagFull :: Typeable a => GDTag -> TagHandler b a -> StructureParser a
parseTagFull tag handler t@(GDTree (GDLine _ mthisID tag' v) children) =
  if tag /= tag' then return$ Left t else do
    r <- case v of
      Nothing -> Right <$> (handler.(first Right)$ parseCont mempty children)
      Just (GDXRefIDV xref) -> do
        Right <$> (handler (Left (GDXRef xref), children))
      Just (GDLineItemV l1) ->
        Right <$> (handler.(first Right)$ parseCont l1 children)
    case mthisID of
      Nothing -> return ()
      Just thisID -> addReference thisID r
    return r

parseCont :: GDLineItem -> [GDTree] -> ([(Maybe GDEscape, T.Text)], [GDTree])
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

