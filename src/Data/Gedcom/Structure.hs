{-|
Module: Data.Gedcom.Structure
Description: GEDCOM data structures
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC

This module contains the GEDCOM records.  Many fields contain references to
other records, which can be dereferenced by 'Data.Gedcom.gdLookup'

-}
module Data.Gedcom.Structure (
-- * Top level structures
Gedcom (..),
Header (..),
GedcomFormat (..),
GedcomForm (..),
HeaderSource (..),
Corp (..),
HeaderSourceData (..),

-- * Records
Family (..),
Individual (..),
Multimedia (..),
Note (..),
Repository (..),
Source (..),
Submission (..),
Submitter (..),

-- * Substructures
SourceData (..),
SourceRecordedEvent (..),
Association (..),
ChildToFamilyLink (..),
SpouseToFamilyLink (..),
EventDetail (..),
FamilyEventDetail (..),
FamilyEvent (..),
IndividualEventDetail (..),
IndividualEvent (..),
Place (..),
PersonalName (..),
PhoneticName (..),
RomanizedName (..),
PersonalNamePieces (..),
IndividualAttribute (..),
IndividualAttributeType (..),
SourceCitation (..),
RepositoryCitation (..),
Address (..),
ContactDetails (..),
MultimediaFile (..),

-- * Values
MultimediaFormat (..),
MultimediaFileFormat (..),
MultimediaType (..),
NameType (..),
DateValue (..),
DateApprox (..),
DateRange (..),
FamilyEventType (..),
IndividualEventType (..),
EventType (..),
AdoptionDetail (..),
Calendar (..),
CallNumber (..),
ChangeDate (..),
Charset (..),
ChildLinkStatus (..),
Date (..),
DatePeriod (..),
MapCoord (..),
Name (..),
Parent (..),
Pedigree (..),
PhoneticPlaceName (..),
PhoneticType (..),
RestrictionNotice (..),
RomanPlaceName (..),
RomanType (..),
Sex (..),
SourceDescription (..),
UserReference (..),
AFN (..),
Language (..),
Latitude (..),
Longitude (..),
QualityAssessment (..),
RFN (..),
RIN (..),
Year (..)
) where

import Data.Gedcom.Internal.CoreTypes
import Data.Time.Clock
import qualified Data.Text.All as T

-- | The root structure
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

-- | The header
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

-- | The database format and version number
data GedcomFormat = GedcomFormat {
  gedcomVersion :: T.Text,
  gedcomForm ::  GedcomForm
} deriving Show

-- | The actual database format.  Only the default LineageLinked is supported.
data GedcomForm =
  GedcomLineageLinked | GedcomUnsupported T.Text deriving (Show, Eq)

-- | Where the GEDCOM data is sourced from (e.g. the program that created the
-- file)
data HeaderSource = HeaderSource {
  headerSourceSystemID :: T.Text,
  headerSourceVersion :: Maybe T.Text,
  headerSourceName :: Maybe T.Text,
  headerSourceCorp :: Maybe Corp,
  headerSourceData :: Maybe HeaderSourceData
} deriving Show

-- | Information about a corporation
data Corp = Corp {
  corpName :: T.Text,
  corpAddress :: Maybe Address
} deriving Show

-- | Information about the source of this GEDCOM data
data HeaderSourceData = HeaderSourceData {
  headerSourceDataName :: T.Text,
  headerSourceDataDate :: Maybe UTCTime,
  headerSourceDataCopyright :: Maybe T.Text
} deriving Show

-- | The family record
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

-- | The individual record
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

-- | The multimedia record (for linking multimedia files)
data Multimedia = Multimedia {
  multimediaFile :: [MultimediaFile],
  multimediaTitl :: Maybe T.Text,
  multimediaUserReference :: [UserReference],
  multimediaRIN :: Maybe RIN,
  multimediaNote :: [GDRef Note],
  multimediaSourceCitation :: [SourceCitation],
  multimediaChangeDate :: Maybe ChangeDate
} deriving Show

-- | The note record (for attaching notes to other records)
data Note = Note {
  noteText :: T.Text,
  noteUserReference :: [UserReference],
  noteRIN :: Maybe RIN,
  noteSourceCitation :: [SourceCitation],
  noteChangeDate :: Maybe ChangeDate
} deriving Show

-- | The repository record.  Represents a repository of sources (for example a
-- collection of documents or a physical library)
data Repository = Repository {
  repositoryName :: T.Text,
  repositoryAddress :: Maybe Address,
  repositoryNote :: [GDRef Note],
  repositoryUserReference :: [UserReference],
  repositoryRIN :: Maybe RIN,
  repositoryChangeDate :: Maybe ChangeDate
} deriving Show

-- | The source record.  A source of information that may be cited by other
-- records.
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

-- | The submission record.  Information about this file.
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

-- | The submitter record.  Information about someone who submitted data to
-- this database.
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

-- | Extra data about a source.
data SourceData = SourceData {
  sourceDataEventsRecorded :: [SourceRecordedEvent],
  sourceDataAgency :: Maybe T.Text,
  sourceDataNote :: [GDRef Note]
} deriving Show

-- | Information about what events are recorded in a source.
data SourceRecordedEvent = SourceRecordedEvent {
  sourceRecordedEventType :: [EventType],
  sourceRecordedDate :: Maybe DatePeriod,
  sourceRecordedPlace :: Maybe [T.Text]
} deriving Show

-- | An association between individuals.
data Association = Association {
  associationIndividual :: GDRef Individual,
  associationRelation :: T.Text,
  associationCitation :: [SourceCitation],
  associationNote :: [GDRef Note]
} deriving Show

-- | A link from an individual to a family record where they are registered as
-- a child.
data ChildToFamilyLink = ChildToFamilyLink {
  childLinkFamily :: GDRef Family,
  childLinkPedigree :: Maybe Pedigree,
  childLinkStatus :: Maybe ChildLinkStatus,
  childLinkNote :: [GDRef Note]
} deriving Show

-- | A link from an individual to a family record where they are registered as
-- a spouse.
data SpouseToFamilyLink = SpouseToFamilyLink {
  spouseToFamilyLinkFamily :: GDRef Family,
  spouseToFamilyLinkNote :: [GDRef Note]
} deriving Show

-- | Details about an event.
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

-- | Details about a family event.
data FamilyEventDetail = FamilyEventDetail {
  familyEventDetailAgeHusband :: Maybe Word,
  familyEventDetailAgeWife :: Maybe Word,
  familyEventDetailDetail :: EventDetail
} deriving Show

-- | An event concerning a family.
data FamilyEvent = FamilyEvent {
  familyEventType :: FamilyEventType,
  familyEventDetail :: FamilyEventDetail
} deriving Show

-- | Details about an individual event.
data IndividualEventDetail = IndividualEventDetail {
  individualEventDetailDetail :: EventDetail,
  individualEventDetailAge :: Maybe Word
} deriving Show

-- | An event concerning an individual.
data IndividualEvent = IndividualEvent {
  individualEventType :: IndividualEventType,
  individualEventDetail :: IndividualEventDetail
} deriving Show

-- | A physical place.
data Place = Place {
  placeName :: [T.Text],
  placeForm :: Maybe [T.Text],
  placePhonetic :: Maybe PhoneticPlaceName,
  placeRoman :: Maybe RomanPlaceName,
  placeMap :: Maybe MapCoord,
  placeNote :: [GDRef Note]
} deriving Show

-- | The name of a person.
data PersonalName = PersonalName {
  personalNameName :: Name,
  personalNameType :: Maybe NameType,
  personalNamePieces :: PersonalNamePieces,
  personalNamePhonetic :: [PhoneticName],
  personalNameRoman :: [RomanizedName]
} deriving Show

-- | A phonetic transcription of a person's name.
data PhoneticName = PhoneticName {
  phoneticName :: Name,
  phoneticType :: PhoneticType,
  phoneticPieces :: PersonalNamePieces
} deriving Show

-- | A Roman transliteration of a person's name.
data RomanizedName = RomanizedName {
  romanziedName :: Name,
  romanziedType :: RomanType,
  romanziedPieces :: PersonalNamePieces
} deriving Show

-- | Parts of a person's name.
data PersonalNamePieces = PersonalNamePieces {
  -- | Parts of the name that precede the other names.  Example from the GEDCOM
  -- standard (prefix highlighted in bold):
  --
  -- __Lt. Cmndr.__ Joseph Allen jr.
  namePiecePrefix :: [T.Text],
  -- | Given names
  namePieceGiven :: [T.Text],
  -- | Nicknames
  namePieceNickname :: [T.Text],
  -- | Surname prefixes.  Example from the GEDCOM standard (surname prefix
  -- highlighted in bold):
  --
  -- __de la__ Cruz
  namePieceSurnamePrefix :: [T.Text],
  -- | Surname or family names
  namePieceSurname :: [T.Text],
  -- | Parts of the name that come after all other names.  Example from the
  -- GEDCOM standard (suffix highlighted in bold)
  --
  -- Lt. Cmndr. Joseph Allen __jr.__
  namePieceSuffix :: [T.Text],
  namePieceNameNote :: [GDRef Note],
  namePirceSourceCitation :: [SourceCitation]
} deriving Show

-- | Extra information about an individual.
data IndividualAttribute = IndividualAttribute
    IndividualAttributeType (IndividualEventDetail)
  deriving Show

-- | Classification of extra information about an individual.
data IndividualAttributeType =
    Caste T.Text               -- ^ Caste.
  | PhysicalDescription T.Text -- ^ Physical description of the individual.
  | Education T.Text      -- ^ Scholastic achievement.
  | NationalID T.Text     -- ^ National id number.
  | NationalOrigin T.Text -- ^ National or tribal origin.
  | NChildren Word        -- ^ Number of children.
  | NMarriages Word       -- ^ Number of marriages.
  | Occupation T.Text     -- ^ Occupation.
  | Possessions T.Text    -- ^ Property the individual owned.
  | Religion T.Text       -- ^ Religious affiliation.
  | ResidesAt             -- ^ Place of residence.
  | SocialSecurity T.Text -- ^ Social security number.
  | Title T.Text          -- ^ Title of nobility.
  | Fact T.Text           -- ^ None of the above.
  deriving Show

-- | Citation of source material.
data SourceCitation = SourceCitation {
  -- | Either a description of the source, or a reference to a 'Source' record
  -- which describes the source in more detail.
  citeSource :: Either SourceDescription (GDRef Source),
  citePage :: Maybe T.Text,
  citeMultimedia :: [GDRef Multimedia],
  citeNote :: [GDRef Note],
  citeQuality :: Maybe QualityAssessment
} deriving Show

-- | Citation of a repository of source material.
data RepositoryCitation = RepositoryCitation {
  repoCiteRepository :: Maybe (GDRef Repository),
  repoCiteNote :: [GDRef Note],
  repoCiteCallNumber :: Maybe CallNumber
} deriving Show

-- | An address
data Address = Address {
  addressLines :: T.Text,
  addressCity :: Maybe T.Text,
  addressState :: Maybe T.Text,
  addressPostcode :: Maybe T.Text,
  addressCountry :: Maybe T.Text,
  addressContact :: ContactDetails
} deriving Show

-- | Contact details associated with an 'Address'
data ContactDetails = ContactDetails {
  addressPhone :: [T.Text],
  addressEmail :: [T.Text],
  addressFax :: [T.Text],
  addressWWW :: [T.Text]
} deriving Show

-- | Information about a multimedia file
data MultimediaFile = MultimediaFile {
  multimediaFileLink :: T.Text,
  multimediaFileFormat :: MultimediaFormat,
  multimediaTitle :: Maybe T.Text
} deriving Show

-- | Information about a multimedia format.
data MultimediaFormat =
  MultimediaFormat MultimediaFileFormat (Maybe MultimediaType) deriving Show

-- | Supported multimedia file formats.
data MultimediaFileFormat =
    MF_BMP
  | MF_GIF
  | MF_JPG
  | MF_OLE
  | MF_PCX
  | MF_TIF
  | MF_WAV
  | MF_OTHER T.Text -- ^ Any other file type.
  deriving Show

-- | The kind of multimedia (not necessarily a computer file).
data MultimediaType =
    MT_AUDIO | MT_BOOK | MT_CARD | MT_ELECTRONIC | MT_FICHE
  | MT_FILM | MT_MAGAZINE | MT_MANUSCRIPT | MT_MAP | MT_NEWSPAPER
  | MT_PHOTO | MT_TOMBSTONE | MT_VIDEO
  | MT_OTHER T.Text -- ^ Any other kind of multimedia.
  deriving Show

-- | Name types.
data NameType =
  AKA | BirthName | Immigrant | Maiden | Married
  | NameType T.Text -- ^ Any other kind of name.
  deriving Show

-- | Dates, including date ranges and approximate dates.
data DateValue = DateV Date
  | DateApproxV DateApprox
  | DatePeriodV DatePeriod
  | DateRangeV DateRange
  | DatePhrase (Maybe Date) T.Text -- ^ A date in a format that doesn't fit the other categories.  May include an interpretation in the optional 'Date' field.
  deriving Show

-- | A date that is only approximately known.
data DateApprox = DateAbout Date
  | DateCalculated Date
  | DateEstimated Date deriving Show

-- | A range of dates.
data DateRange = DateBefore Date
  | DateAfter Date
  | DateBetween Date Date deriving Show

-- | Recognised types of events concerning families.
data FamilyEventType =
    Annuled | FamCensus | Divorce | DivorceFiled | Engagement
  | MarriageBann | MarriageContract | Marriage | MarriageLicense
  | MarriageSettlement | Residence
  | FamilyEventType T.Text -- ^ Any other kind of event.
  deriving Show

-- | Recognised types of events concerning individuals.
data IndividualEventType =
    Birth (Maybe (GDRef Family)) | Christening (Maybe (GDRef Family))
  | Death | Burial
  | Cremation | Adoption (Maybe AdoptionDetail)
  | Baptism | BarMitzvah
  | BasMitzvah | Blessing | ChristeningAdult | Confirmation | FirstCommunion
  | Ordination | Naturalization | Emigration | Immigration | IndvCensus
  | Probate | Will | Graduation | Retirement
  | IndividualEventType T.Text -- ^ Any other kind of event.
  deriving Show

-- | Recognised event types.
data EventType =
    Census  -- ^ Family of individual census.
  | FamilyEventTypeV FamilyEventType -- ^ A family event
  | IndividualEventTypeV IndividualEventType -- ^ An individual event
  | EventType T.Text -- ^ Some other kind of event.
  deriving Show

-- | Information about an adoption, including the family the subject was
-- adopted into and which parent(s) adopted the child.
data AdoptionDetail =
  AdoptionDetail (GDRef Family) (Maybe Parent) deriving Show
-- | A calendar
data Calendar = Gregorian | Julian | Hebrew | French deriving Show
-- | A call number for citations
data CallNumber = CallNumber T.Text (Maybe MultimediaType) deriving Show
-- | The date a record was last changed
data ChangeDate = ChangeDate UTCTime (Maybe (GDRef Note)) deriving Show
-- | A character set and optional version (the version is unused so far as I am
-- aware)
data Charset = Charset T.Text (Maybe T.Text) deriving Show
-- | Metadata about a child link
data ChildLinkStatus = Challenged | Disproved | Proven deriving Show
-- | A date.  The format is day \/ month \/ year and months are numbered 1 to
-- 12 (or 1 to 13 for certain calendars).
data Date = Date Calendar (Maybe Word) (Maybe Word) Year deriving Show
-- | A range of dates
data DatePeriod =
    DateFrom Date (Maybe Date) -- ^ A range of dates with a start date and optional end date.
  | DateTo Date -- ^ A range of dates with only the end date specified.
  deriving Show
-- | A global location in longitude and latitude.
data MapCoord = MapCoord Longitude Latitude deriving Show
-- | A personal name.  The first field is the full name of the individual.  The
-- second field contains just the surname of the individual (or Nothing if
-- unspecified)
data Name = Name T.Text (Maybe T.Text) deriving Show
-- | Which parent?
data Parent = Husband | Wife | BothParents deriving Show
-- | How a child is associated with his\/her family.
data Pedigree = Adopted | ByBirth | Foster | Sealing deriving Show
-- | A phonetic transcription of a place name.
data PhoneticPlaceName = PhoneticPlaceName PhoneticType [T.Text] deriving Show
-- | The type of phonetic transcription.
data PhoneticType =
    Kana    -- ^ Japanese kana
  | Hangul  -- ^ Korean hanguel
  | PhoneticType T.Text -- ^ Something else
  deriving Show
-- | Privacy restrictions associated with a record.
data RestrictionNotice = Confidential | Locked | Privacy deriving Show
-- | A Roman transliteration of a place name.
data RomanPlaceName = RomanPlaceName RomanType [T.Text] deriving Show
-- | The type of Roman transliteration.
data RomanType =
    Pinyin             -- ^ Chinese Pinyin
  | Romaji             -- ^ Japanese Romaji
  | WadeGiles          -- ^ Chinese Wade-Giles
  | RomanType T.Text   -- ^ Something else
  deriving Show
-- | Sex.
data Sex = Male | Female | Undetermined deriving Show
-- | Description of a source.  The first field contains a description of the
-- source.  The second field contains sections of text from the source.
data SourceDescription = SourceDescription T.Text [T.Text] deriving Show
-- | Custom reference added by the creator of this file.  The format of this
-- reference is not standardized.
data UserReference = UserReference T.Text (Maybe T.Text) deriving Show

-- | A natural language.
newtype Language = Language T.Text deriving Show
-- | Latitude
newtype Latitude = Latitude Double deriving Show
-- | Longitude
newtype Longitude = Longitude Double deriving Show
-- | Assessment of the quality of a source.  A number from 0 to 3 with 0
-- indicating unreliable information and 3 being direct primary evidence.
newtype QualityAssessment = QualityAssessment Word deriving Show
-- | A year in some calendar.  May be negative.
newtype Year = Year Int deriving Show
-- | Automated Record ID.  From the GEDCOM standard:
--
-- "This number is intended to serve as a more sure means of identification of
-- a record for reconciling differences in data between two interfacing
-- systems."
newtype RIN = RIN T.Text deriving Show

-- | Part of the GEDCOM standard, but only for LDS use so far as I am aware.
newtype AFN = AFN T.Text deriving Show

-- | Part of the GEDCOM standard, but only for LDS use so far as I am aware.
newtype RFN = RFN T.Text deriving Show

