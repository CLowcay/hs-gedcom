module Data.Gedcom.Structure where

import Data.Gedcom.Common
import Data.Time.Clock
import qualified Data.Text.All as T

-- The root structure

data Gedcom = Gedcom {
  gedcomHeader :: Header,
  gedcomFamily :: [GDRef Family],
  gedcomIndividual :: [GDRef Individual],
  gedcomMultimedia :: [GDRef Multimedia],
  gedcomNote :: [GDRef Note],
  gedcomRepository :: [GDRef Repository],
  gedcomSource :: [GDRef Source],
  gedcomSubmitter :: [GDRef Submitter]
} deriving Show

-- The header

data Header = Header {
  headerSource :: HeaderSource,
  headerDestination :: Maybe T.Text,
  headerDate :: Maybe UTCTime,
  headerSubmitter :: GDRef Submitter,
  headerSubmission :: Maybe (GDRef Submission),
  headerFile :: Maybe FilePath,
  headerCopyright :: Maybe T.Text,
  headerGedcomFormat :: GedcomFormat,
  headerCharset :: Charset,
  headerLang :: Maybe Language,
  headerPlaceForm :: Maybe [T.Text],
  headerNote :: Maybe T.Text
} deriving Show

data GedcomFormat = GedcomFormat {
  gedcomVersion :: T.Text,
  gedcomForm ::  GedcomForm
} deriving Show

data GedcomForm =
  GedcomLineageLinked | GedcomUnsupported T.Text deriving (Show, Eq)

data HeaderSource = HeaderSource {
  headerSourceSystemID :: T.Text,
  headerSourceVersion :: Maybe T.Text,
  headerSourceName :: Maybe T.Text,
  headerSourceCorp :: Maybe Corp,
  headerSourceData :: Maybe HeaderSourceData
} deriving Show

data Corp = Corp {
  corpName :: T.Text,
  corpAddress :: Maybe Address
} deriving Show

data HeaderSourceData = HeaderSourceData {
  headerSourceDataName :: T.Text,
  headerSourceDataDate :: Maybe UTCTime,
  headerSourceDataCopyright :: Maybe T.Text
} deriving Show

-- records

data Family = Family {
  familyRestrictionNotice :: Maybe RestrictionNotice,
  familyEvent :: [FamilyEvent],
  familyHusband :: Maybe (GDRef Individual),
  familyWife :: Maybe (GDRef Individual),
  familyChildren :: [GDRef Individual],
  familyTotalChildren :: Maybe Word,
  familySubitter :: [GDRef Submitter],
  familyUserReference :: [UserReference],
  familyRIN :: Maybe RIN,
  familyChangeDate :: Maybe ChangeDate,
  familyNote :: [GDRef Note],
  familySourceCitation :: [SourceCitation],
  familyMultimedia :: [GDRef Multimedia]
} deriving Show

data Individual = Individual {
  individualRestrictionNotice :: Maybe RestrictionNotice,
  individualName :: Maybe PersonalName,
  individualSex :: Maybe Sex,
  individualEvent :: [IndividualEvent],
  individualAttribute :: [IndividualAttribute],
  individualChildToFamilyLink :: [ChildToFamilyLink],
  individualSpouseToFamilyLink :: [SpouseToFamilyLink],
  individualSubmitter :: [GDRef Submitter],
  individualAssociation :: [Association],
  individualAlias :: [GDRef Individual],
  individualAncestorInterest :: [GDRef Submitter],
  individualDescendantInterest :: [GDRef Submitter],
  individualRFN :: Maybe RFN,
  individualAFN :: Maybe AFN,
  individualUserReference :: [UserReference],
  individualRIN :: Maybe RIN,
  individualChangeDate :: Maybe ChangeDate,
  individualNote :: [GDRef Note],
  individualSourceCitation :: [SourceCitation],
  individualMultimedia :: [GDRef Multimedia]
} deriving Show

data Multimedia = Multimedia {
  multimediaFile :: [MultimediaFile],
  multimediaTitl :: Maybe T.Text,
  multimediaUserReference :: [UserReference],
  multimediaRIN :: Maybe RIN,
  multimediaNote :: [GDRef Note],
  multimediaSourceCitation :: [SourceCitation],
  multimediaChangeDate :: Maybe ChangeDate
} deriving Show

data Note = Note {
  noteText :: T.Text,
  noteUserReference :: [UserReference],
  noteRIN :: Maybe RIN,
  noteSourceCitation :: [SourceCitation],
  noteChangeDate :: Maybe ChangeDate
} deriving Show

data Repository = Repository {
  repositoryName :: T.Text,
  repositoryAddress :: Maybe Address,
  repositoryNote :: [GDRef Note],
  repositoryUserReference :: [UserReference],
  repositoryRIN :: Maybe RIN,
  repositoryChangeDate :: Maybe ChangeDate
} deriving Show

data Source = Source {
  sourceData :: Maybe SourceData,
  sourceAuthor :: Maybe T.Text,
  sourceTitle :: Maybe T.Text,
  sourceShortTitle :: Maybe T.Text,
  sourcePublicationFacts :: Maybe T.Text,
  sourceText :: Maybe T.Text,
  sourceRepositoryCitations :: [RepositoryCitation],
  sourceUserReference :: [UserReference],
  sourceRIN :: Maybe RIN,
  sourceChangeDate :: Maybe ChangeDate,
  sourceNote :: [GDRef Note],
  sourceMultimedia :: [GDRef Multimedia]
} deriving Show

data Submission = Submission {
  submissionSubmitter :: Maybe (GDRef Submitter),
  submissionFamilyFile :: Maybe T.Text,
  submissionTempleCode :: Maybe T.Text,
  submissionAncestorGenerations :: Maybe Word,
  submissionDescendentGenerations :: Maybe Word,
  submissionOrdinanceProcessing :: Maybe Bool,
  submissionRIN :: Maybe RIN,
  submissionNote :: [GDRef Note],
  submissionChangeDate :: Maybe ChangeDate
} deriving Show

data Submitter = Submitter {
  submitterName :: Name,
  submitterAddress :: Maybe Address,
  submitterMedia :: Maybe (GDRef Multimedia),
  submitterLang :: [Language],
  submitterRFN :: Maybe RFN,
  submitterRIN :: Maybe RIN,
  submitterNote :: [GDRef Note],
  submitterChangeDate :: Maybe ChangeDate
} deriving Show

-- Substructures
data SourceData = SourceData {
  sourceDataEventsRecorded :: [SourceRecordedEvent],
  sourceDataAgency :: Maybe T.Text,
  sourceDataNote :: [GDRef Note]
} deriving Show

data SourceRecordedEvent = SourceRecordedEvent {
  sourceRecordedEventType :: [EventType],
  sourceRecordedDate :: Maybe DatePeriod,
  sourceRecordedPlace :: Maybe [T.Text]
} deriving Show

data Association = Association {
  associationIndividual :: GDRef Individual,
  associationRelation :: T.Text,
  associationCitation :: [SourceCitation],
  associationNote :: [GDRef Note]
} deriving Show

data ChildToFamilyLink = ChildToFamilyLink {
  childLinkFamily :: GDRef Family,
  childLinkPedigree :: Maybe Pedigree,
  childLinkStatus :: Maybe ChildLinkStatus,
  childLinkNote :: [GDRef Note]
} deriving Show

data SpouseToFamilyLink = SpouseToFamilyLink {
  spouseToFamilyLinkFamily :: GDRef Family,
  spouseToFamilyLinkNote :: [GDRef Note]
} deriving Show

data EventDetail = EventDetail {
  eventDetailType :: Maybe T.Text,
  eventDetailDate :: Maybe DateValue,
  eventDetailPlace :: Maybe Place,
  eventDetailAddress :: Maybe Address,
  eventDetailAgency :: Maybe T.Text,
  eventDetailReligion :: Maybe T.Text,
  eventDetailCause :: Maybe T.Text,
  eventDetailRestrictionNotice :: Maybe RestrictionNotice,
  eventDetailNote :: [GDRef Note],
  eventDetailSourceCitation :: [SourceCitation],
  eventDetailMultimedia :: [GDRef Multimedia]
} deriving Show

data FamilyEventDetail = FamilyEventDetail {
  familyEventDetailAgeHusband :: Maybe Word,
  familyEventDetailAgeWife :: Maybe Word,
  familyEventDetailDetail :: EventDetail
} deriving Show

data FamilyEvent = FamilyEvent {
  familyEventType :: FamilyEventType,
  familyEventDetail :: FamilyEventDetail
} deriving Show

data IndividualEventDetail = IndividualEventDetail {
  individualEventDetailDetail :: EventDetail,
  individualEventDetailAge :: Maybe Word
} deriving Show

data IndividualEvent = IndividualEvent {
  individualEventType :: IndividualEventType,
  individualEventDetail :: IndividualEventDetail
} deriving Show

data Place = Place {
  placeName :: [T.Text],
  placeForm :: Maybe [T.Text],
  placePhonetic :: Maybe PhoneticPlaceName,
  placeRoman :: Maybe RomanPlaceName,
  placeMap :: Maybe MapCoord,
  placeNote :: [GDRef Note]
} deriving Show

data PersonalName = PersonalName {
  personalNameName :: Name,
  personalNameType :: Maybe NameType,
  personalNamePieces :: PersonalNamePieces,
  personalNamePhonetic :: [PhoneticName],
  personalNameRoman :: [RomanizedName]
} deriving Show

data PhoneticName = PhoneticName {
  phoneticName :: Name,
  phoneticType :: PhoneticType,
  phoneticPieces :: PersonalNamePieces
} deriving Show

data RomanizedName = RomanizedName {
  romanziedName :: Name,
  romanziedType :: RomanType,
  romanziedPieces :: PersonalNamePieces
} deriving Show

data PersonalNamePieces = PersonalNamePieces {
  namePiecePrefix :: [T.Text],
  namePieceGiven :: [T.Text],
  namePieceNickname :: [T.Text],
  namePieceSurnamePrefix :: [T.Text],
  namePieceSurname :: [T.Text],
  namePieceSuffix :: [T.Text],
  namePieceNameNote :: [GDRef Note],
  namePirceSourceCitation :: [SourceCitation]
} deriving Show

data IndividualAttribute = IndividualAttribute
    IndividualAttributeType (IndividualEventDetail)
  deriving Show

data IndividualAttributeType =
    Caste T.Text
  | PhysicalDescription T.Text
  | Education T.Text
  | NationalID T.Text
  | NationalOrigin T.Text
  | NChildren Word
  | NMarriages Word
  | Occupation T.Text
  | Possessions T.Text
  | Religion T.Text
  | ResidesAt
  | SocialSecurity T.Text
  | Title T.Text
  | Fact T.Text deriving Show

data SourceCitation = SourceCitation {
  citeSource :: Either SourceDescription (GDRef Source),
  citePage :: Maybe T.Text,
  citeMultimedia :: [GDRef Multimedia],
  citeNote :: [GDRef Note],
  citeQuality :: Maybe QualityAssessment
} deriving Show

data RepositoryCitation = RepositoryCitation {
  repoCiteRepository :: Maybe (GDRef Repository),
  repoCiteNote :: [GDRef Note],
  repoCiteCallNumber :: Maybe CallNumber
} deriving Show

data Address = Address {
  addressLines :: T.Text,
  addressCity :: Maybe T.Text,
  addressState :: Maybe T.Text,
  addressPostcode :: Maybe T.Text,
  addressCountry :: Maybe T.Text,
  addressContact :: ContactDetails
} deriving Show

data ContactDetails = ContactDetails {
  addressPhone :: [T.Text],
  addressEmail :: [T.Text],
  addressFax :: [T.Text],
  addressWWW :: [T.Text]
} deriving Show

data MultimediaFile = MultimediaFile {
  multimediaFileLink :: T.Text,
  multimediaFileFormat :: MultimediaFormat,
  multimediaTitle :: Maybe T.Text
} deriving Show

-- Values

data MultimediaFormat =
  MultimediaFormat MultimediaFileFormat (Maybe MultimediaType) deriving Show

data MultimediaFileFormat =
    MF_BMP | MF_GIF | MF_JPG | MF_OLE
  | MF_PCX | MF_TIF | MF_WAV | MF_OTHER T.Text deriving Show

data MultimediaType =
    MT_AUDIO | MT_BOOK | MT_CARD | MT_ELECTRONIC | MT_FICHE
  | MT_FILM | MT_MAGAZINE | MT_MANUSCRIPT | MT_MAP | MT_NEWSPAPER
  | MT_PHOTO | MT_TOMBSTONE | MT_VIDEO | MT_OTHER T.Text deriving Show

data NameType =
  AKA | BirthName | Immigrant | Maiden | Married | NameType T.Text
  deriving Show

data DateValue = DateV Date
  | DateApproxV DateApprox
  | DatePeriodV DatePeriod
  | DateRangeV DateRange
  | DatePhrase (Maybe Date) T.Text deriving Show

data DateApprox = DateAbout Date
  | DateCalculated Date
  | DateEstimated Date deriving Show

data DateRange = DateBefore Date
  | DateAfter Date
  | DateBetween Date Date deriving Show

data FamilyEventType =
    Annuled | FamCensus | Divorce | DivorceFiled | Engagement
  | MarriageBann | MarriageContract | Marriage | MarriageLicense
  | MarriageSettlement | Residence | FamilyEventType T.Text
  deriving Show

data IndividualEventType =
    Birth (Maybe (GDRef Family)) | Christening (Maybe (GDRef Family))
  | Death | Burial
  | Cremation | Adoption (Maybe AdoptionDetail)
  | Baptism | BarMitzvah
  | BasMitzvah | Blessing | ChristeningAdult | Confirmation | FirstCommunion
  | Ordination | Naturalization | Emigration | Immigration | IndvCensus
  | Probate | Will | Graduation | Retirement | IndividualEventType T.Text
  deriving Show

data EventType =
    Census
  | FamilyEventTypeV FamilyEventType
  | IndividualEventTypeV IndividualEventType
  | EventType T.Text
  deriving Show

data AdoptionDetail =
  AdoptionDetail (GDRef Family) (Maybe Parent) deriving Show
data Calendar = Gregorian | Julian | Hebrew | French deriving Show
data CallNumber = CallNumber T.Text (Maybe MultimediaType) deriving Show
data ChangeDate = ChangeDate UTCTime (Maybe (GDRef Note)) deriving Show
data Charset = Charset T.Text (Maybe T.Text) deriving Show
data ChildLinkStatus = Challenged | Disproved | Proven deriving Show
data Date = Date Calendar (Maybe Word) (Maybe Word) Year deriving Show
data DatePeriod = DateFrom Date (Maybe Date) | DateTo Date deriving Show
data MapCoord = MapCoord Longitude Latitude deriving Show
data Name = Name T.Text (Maybe T.Text) deriving Show -- Name fullname surname_only
data Parent = Husband | Wife | BothParents deriving Show
data Pedigree = Adopted | ByBirth | Foster | Sealing deriving Show
data PhoneticPlaceName = PhoneticPlaceName PhoneticType [T.Text] deriving Show
data PhoneticType = Kana | Hangul | PhoneticType T.Text deriving Show
data RestrictionNotice = Confidential | Locked | Privacy deriving Show
data RomanPlaceName = RomanPlaceName RomanType [T.Text] deriving Show
data RomanType = Pinyin | Romaji | WadeGiles | RomanType T.Text deriving Show
data Sex = Male | Female | Undetermined deriving Show
data SourceDescription = SourceDescription T.Text [T.Text] deriving Show
data UserReference = UserReference T.Text (Maybe T.Text) deriving Show

newtype AFN = AFN T.Text deriving Show
newtype Language = Language T.Text deriving Show
newtype Latitude = Latitude Double deriving Show
newtype Longitude = Longitude Double deriving Show
newtype QualityAssessment = QualityAssessment Word deriving Show
newtype RFN = RFN T.Text deriving Show
newtype RIN = RIN T.Text deriving Show
newtype Year = Year Int deriving Show

