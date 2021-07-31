-- |
-- Module: Data.Gedcom.Structure
-- Description: GEDCOM data structures
-- Copyright: (c) Callum Lowcay, 2017 - 2021
-- License: BSD3
-- Maintainer: cwslowcay@gmail.com
-- Stability: experimental
-- Portability: GHC
--
-- This module contains the GEDCOM records.  Many fields contain references to
-- other records, which can be dereferenced by 'Data.Gedcom.gdLookup'
module Data.Gedcom.Structure
  ( -- * Top level structures
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
    Year (..),
  )
where

import Data.Gedcom.Internal.CoreTypes (GDRef)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | The root structure
data Gedcom = Gedcom
  { gedcomHeader :: Header,
    gedcomFamily :: [GDRef Family],
    gedcomIndividual :: [GDRef Individual],
    gedcomMultimedia :: [GDRef Multimedia],
    gedcomNote :: [GDRef Note],
    gedcomRepository :: [GDRef Repository],
    gedcomSource :: [GDRef Source],
    gedcomSubmitter :: [GDRef Submitter]
  }
  deriving (Show)

-- | The header
data Header = Header
  { headerSource :: HeaderSource,
    headerDestination :: Maybe Text,
    headerDate :: Maybe UTCTime,
    headerSubmitter :: GDRef Submitter,
    headerSubmission :: Maybe (GDRef Submission),
    headerFile :: Maybe FilePath,
    headerCopyright :: Maybe Text,
    headerGedcomFormat :: GedcomFormat,
    headerCharset :: Charset,
    headerLang :: Maybe Language,
    headerPlaceForm :: Maybe [Text],
    headerNote :: Maybe Text
  }
  deriving (Show)

-- | The database format and version number
data GedcomFormat = GedcomFormat
  { gedcomVersion :: Text,
    gedcomForm :: GedcomForm
  }
  deriving (Show)

-- | The actual database format.  Only the default LineageLinked is supported.
data GedcomForm
  = GedcomLineageLinked
  | GedcomUnsupported Text
  deriving (Show, Eq)

-- | Where the GEDCOM data is sourced from (e.g. the program that created the
-- file)
data HeaderSource = HeaderSource
  { headerSourceSystemID :: Text,
    headerSourceVersion :: Maybe Text,
    headerSourceName :: Maybe Text,
    headerSourceCorp :: Maybe Corp,
    headerSourceData :: Maybe HeaderSourceData
  }
  deriving (Show)

-- | Information about a corporation
data Corp = Corp
  { corpName :: Text,
    corpAddress :: Maybe Address
  }
  deriving (Show)

-- | Information about the source of this GEDCOM data
data HeaderSourceData = HeaderSourceData
  { headerSourceDataName :: Text,
    headerSourceDataDate :: Maybe UTCTime,
    headerSourceDataCopyright :: Maybe Text
  }
  deriving (Show)

-- | The family record
data Family = Family
  { familyRestrictionNotice :: Maybe RestrictionNotice,
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
  }
  deriving (Show)

-- | The individual record
data Individual = Individual
  { individualRestrictionNotice :: Maybe RestrictionNotice,
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
  }
  deriving (Show)

-- | The multimedia record (for linking multimedia files)
data Multimedia = Multimedia
  { multimediaFile :: [MultimediaFile],
    multimediaTitl :: Maybe Text,
    multimediaUserReference :: [UserReference],
    multimediaRIN :: Maybe RIN,
    multimediaNote :: [GDRef Note],
    multimediaSourceCitation :: [SourceCitation],
    multimediaChangeDate :: Maybe ChangeDate
  }
  deriving (Show)

-- | The note record (for attaching notes to other records)
data Note = Note
  { noteText :: Text,
    noteUserReference :: [UserReference],
    noteRIN :: Maybe RIN,
    noteSourceCitation :: [SourceCitation],
    noteChangeDate :: Maybe ChangeDate
  }
  deriving (Show)

-- | The repository record.  Represents a repository of sources (for example a
-- collection of documents or a physical library)
data Repository = Repository
  { repositoryName :: Text,
    repositoryAddress :: Maybe Address,
    repositoryNote :: [GDRef Note],
    repositoryUserReference :: [UserReference],
    repositoryRIN :: Maybe RIN,
    repositoryChangeDate :: Maybe ChangeDate
  }
  deriving (Show)

-- | The source record.  A source of information that may be cited by other
-- records.
data Source = Source
  { sourceData :: Maybe SourceData,
    sourceAuthor :: Maybe Text,
    sourceTitle :: Maybe Text,
    sourceShortTitle :: Maybe Text,
    sourcePublicationFacts :: Maybe Text,
    sourceText :: Maybe Text,
    sourceRepositoryCitations :: [RepositoryCitation],
    sourceUserReference :: [UserReference],
    sourceRIN :: Maybe RIN,
    sourceChangeDate :: Maybe ChangeDate,
    sourceNote :: [GDRef Note],
    sourceMultimedia :: [GDRef Multimedia]
  }
  deriving (Show)

-- | The submission record.  Information about this file.
data Submission = Submission
  { submissionSubmitter :: Maybe (GDRef Submitter),
    submissionFamilyFile :: Maybe Text,
    submissionTempleCode :: Maybe Text,
    submissionAncestorGenerations :: Maybe Word,
    submissionDescendentGenerations :: Maybe Word,
    submissionOrdinanceProcessing :: Maybe Bool,
    submissionRIN :: Maybe RIN,
    submissionNote :: [GDRef Note],
    submissionChangeDate :: Maybe ChangeDate
  }
  deriving (Show)

-- | The submitter record.  Information about someone who submitted data to
-- this database.
data Submitter = Submitter
  { submitterName :: Name,
    submitterAddress :: Maybe Address,
    submitterMedia :: Maybe (GDRef Multimedia),
    submitterLang :: [Language],
    submitterRFN :: Maybe RFN,
    submitterRIN :: Maybe RIN,
    submitterNote :: [GDRef Note],
    submitterChangeDate :: Maybe ChangeDate
  }
  deriving (Show)

-- | Extra data about a source.
data SourceData = SourceData
  { sourceDataEventsRecorded :: [SourceRecordedEvent],
    sourceDataAgency :: Maybe Text,
    sourceDataNote :: [GDRef Note]
  }
  deriving (Show)

-- | Information about what events are recorded in a source.
data SourceRecordedEvent = SourceRecordedEvent
  { sourceRecordedEventType :: [EventType],
    sourceRecordedDate :: Maybe DatePeriod,
    sourceRecordedPlace :: Maybe [Text]
  }
  deriving (Show)

-- | An association between individuals.
data Association = Association
  { associationIndividual :: GDRef Individual,
    associationRelation :: Text,
    associationCitation :: [SourceCitation],
    associationNote :: [GDRef Note]
  }
  deriving (Show)

-- | A link from an individual to a family record where they are registered as
-- a child.
data ChildToFamilyLink = ChildToFamilyLink
  { childLinkFamily :: GDRef Family,
    childLinkPedigree :: Maybe Pedigree,
    childLinkStatus :: Maybe ChildLinkStatus,
    childLinkNote :: [GDRef Note]
  }
  deriving (Show)

-- | A link from an individual to a family record where they are registered as
-- a spouse.
data SpouseToFamilyLink = SpouseToFamilyLink
  { spouseToFamilyLinkFamily :: GDRef Family,
    spouseToFamilyLinkNote :: [GDRef Note]
  }
  deriving (Show)

-- | Details about an event.
data EventDetail = EventDetail
  { eventDetailType :: Maybe Text,
    eventDetailDate :: Maybe DateValue,
    eventDetailPlace :: Maybe Place,
    eventDetailAddress :: Maybe Address,
    eventDetailAgency :: Maybe Text,
    eventDetailReligion :: Maybe Text,
    eventDetailCause :: Maybe Text,
    eventDetailRestrictionNotice :: Maybe RestrictionNotice,
    eventDetailNote :: [GDRef Note],
    eventDetailSourceCitation :: [SourceCitation],
    eventDetailMultimedia :: [GDRef Multimedia]
  }
  deriving (Show)

-- | Details about a family event.
data FamilyEventDetail = FamilyEventDetail
  { familyEventDetailAgeHusband :: Maybe Word,
    familyEventDetailAgeWife :: Maybe Word,
    familyEventDetailDetail :: EventDetail
  }
  deriving (Show)

-- | An event concerning a family.
data FamilyEvent = FamilyEvent
  { familyEventType :: FamilyEventType,
    familyEventDetail :: FamilyEventDetail
  }
  deriving (Show)

-- | Details about an individual event.
data IndividualEventDetail = IndividualEventDetail
  { individualEventDetailDetail :: EventDetail,
    individualEventDetailAge :: Maybe Word
  }
  deriving (Show)

-- | An event concerning an individual.
data IndividualEvent = IndividualEvent
  { individualEventType :: IndividualEventType,
    individualEventDetail :: IndividualEventDetail
  }
  deriving (Show)

-- | A physical place.
data Place = Place
  { placeName :: [Text],
    placeForm :: Maybe [Text],
    placePhonetic :: Maybe PhoneticPlaceName,
    placeRoman :: Maybe RomanPlaceName,
    placeMap :: Maybe MapCoord,
    placeNote :: [GDRef Note]
  }
  deriving (Show)

-- | The name of a person.
data PersonalName = PersonalName
  { personalNameName :: Name,
    personalNameType :: Maybe NameType,
    personalNamePieces :: PersonalNamePieces,
    personalNamePhonetic :: [PhoneticName],
    personalNameRoman :: [RomanizedName]
  }
  deriving (Show)

-- | A phonetic transcription of a person's name.
data PhoneticName = PhoneticName
  { phoneticName :: Name,
    phoneticType :: PhoneticType,
    phoneticPieces :: PersonalNamePieces
  }
  deriving (Show)

-- | A Roman transliteration of a person's name.
data RomanizedName = RomanizedName
  { romanziedName :: Name,
    romanziedType :: RomanType,
    romanziedPieces :: PersonalNamePieces
  }
  deriving (Show)

-- | Parts of a person's name.
data PersonalNamePieces = PersonalNamePieces
  { -- | Parts of the name that precede the other names.  Example from the GEDCOM
    -- standard (prefix highlighted in bold):
    --
    -- __Lt. Cmndr.__ Joseph Allen jr.
    namePiecePrefix :: [Text],
    -- | Given names
    namePieceGiven :: [Text],
    -- | Nicknames
    namePieceNickname :: [Text],
    -- | Surname prefixes.  Example from the GEDCOM standard (surname prefix
    -- highlighted in bold):
    --
    -- __de la__ Cruz
    namePieceSurnamePrefix :: [Text],
    -- | Surname or family names
    namePieceSurname :: [Text],
    -- | Parts of the name that come after all other names.  Example from the
    -- GEDCOM standard (suffix highlighted in bold)
    --
    -- Lt. Cmndr. Joseph Allen __jr.__
    namePieceSuffix :: [Text],
    namePieceNameNote :: [GDRef Note],
    namePirceSourceCitation :: [SourceCitation]
  }
  deriving (Show)

-- | Extra information about an individual.
data IndividualAttribute
  = IndividualAttribute
      IndividualAttributeType
      IndividualEventDetail
  deriving (Show)

-- | Classification of extra information about an individual.
data IndividualAttributeType
  = -- | Caste.
    Caste Text
  | -- | Physical description of the individual.
    PhysicalDescription Text
  | -- | Scholastic achievement.
    Education Text
  | -- | National id number.
    NationalID Text
  | -- | National or tribal origin.
    NationalOrigin Text
  | -- | Number of children.
    NChildren Word
  | -- | Number of marriages.
    NMarriages Word
  | -- | Occupation.
    Occupation Text
  | -- | Property the individual owned.
    Possessions Text
  | -- | Religious affiliation.
    Religion Text
  | -- | Place of residence.
    ResidesAt
  | -- | Social security number.
    SocialSecurity Text
  | -- | Title of nobility.
    Title Text
  | -- | None of the above.
    Fact Text
  deriving (Show)

-- | Citation of source material.
data SourceCitation = SourceCitation
  { -- | Either a description of the source, or a reference to a 'Source' record
    -- which describes the source in more detail.
    citeSource :: Either SourceDescription (GDRef Source),
    citePage :: Maybe Text,
    citeMultimedia :: [GDRef Multimedia],
    citeNote :: [GDRef Note],
    citeQuality :: Maybe QualityAssessment
  }
  deriving (Show)

-- | Citation of a repository of source material.
data RepositoryCitation = RepositoryCitation
  { repoCiteRepository :: Maybe (GDRef Repository),
    repoCiteNote :: [GDRef Note],
    repoCiteCallNumber :: Maybe CallNumber
  }
  deriving (Show)

-- | An address
data Address = Address
  { addressLines :: Text,
    addressCity :: Maybe Text,
    addressState :: Maybe Text,
    addressPostcode :: Maybe Text,
    addressCountry :: Maybe Text,
    addressContact :: ContactDetails
  }
  deriving (Show)

-- | Contact details associated with an 'Address'
data ContactDetails = ContactDetails
  { addressPhone :: [Text],
    addressEmail :: [Text],
    addressFax :: [Text],
    addressWWW :: [Text]
  }
  deriving (Show)

-- | Information about a multimedia file
data MultimediaFile = MultimediaFile
  { multimediaFileLink :: Text,
    multimediaFileFormat :: MultimediaFormat,
    multimediaTitle :: Maybe Text
  }
  deriving (Show)

-- | Information about a multimedia format.
data MultimediaFormat
  = MultimediaFormat MultimediaFileFormat (Maybe MultimediaType)
  deriving (Show)

-- | Supported multimedia file formats.
data MultimediaFileFormat
  = MF_BMP
  | MF_GIF
  | MF_JPG
  | MF_OLE
  | MF_PCX
  | MF_TIF
  | MF_WAV
  | -- | Any other file type.
    MF_OTHER Text
  deriving (Show)

-- | The kind of multimedia (not necessarily a computer file).
data MultimediaType
  = MT_AUDIO
  | MT_BOOK
  | MT_CARD
  | MT_ELECTRONIC
  | MT_FICHE
  | MT_FILM
  | MT_MAGAZINE
  | MT_MANUSCRIPT
  | MT_MAP
  | MT_NEWSPAPER
  | MT_PHOTO
  | MT_TOMBSTONE
  | MT_VIDEO
  | -- | Any other kind of multimedia.
    MT_OTHER Text
  deriving (Show)

-- | Name types.
data NameType
  = AKA
  | BirthName
  | Immigrant
  | Maiden
  | Married
  | -- | Any other kind of name.
    NameType Text
  deriving (Show)

-- | Dates, including date ranges and approximate dates.
data DateValue
  = DateV Date
  | DateApproxV DateApprox
  | DatePeriodV DatePeriod
  | DateRangeV DateRange
  | -- | A date in a format that doesn't fit the other categories.  May include an interpretation in the optional 'Date' field.
    DatePhrase (Maybe Date) Text
  deriving (Show)

-- | A date that is only approximately known.
data DateApprox
  = DateAbout Date
  | DateCalculated Date
  | DateEstimated Date
  deriving (Show)

-- | A range of dates.
data DateRange
  = DateBefore Date
  | DateAfter Date
  | DateBetween Date Date
  deriving (Show)

-- | Recognised types of events concerning families.
data FamilyEventType
  = Annuled
  | FamCensus
  | Divorce
  | DivorceFiled
  | Engagement
  | MarriageBann
  | MarriageContract
  | Marriage
  | MarriageLicense
  | MarriageSettlement
  | Residence
  | -- | Any other kind of event.
    FamilyEventType Text
  deriving (Show)

-- | Recognised types of events concerning individuals.
data IndividualEventType
  = Birth (Maybe (GDRef Family))
  | Christening (Maybe (GDRef Family))
  | Death
  | Burial
  | Cremation
  | Adoption (Maybe AdoptionDetail)
  | Baptism
  | BarMitzvah
  | BasMitzvah
  | Blessing
  | ChristeningAdult
  | Confirmation
  | FirstCommunion
  | Ordination
  | Naturalization
  | Emigration
  | Immigration
  | IndvCensus
  | Probate
  | Will
  | Graduation
  | Retirement
  | -- | Any other kind of event.
    IndividualEventType Text
  deriving (Show)

-- | Recognised event types.
data EventType
  = -- | Family of individual census.
    Census
  | -- | A family event
    FamilyEventTypeV FamilyEventType
  | -- | An individual event
    IndividualEventTypeV IndividualEventType
  | -- | Some other kind of event.
    EventType Text
  deriving (Show)

-- | Information about an adoption, including the family the subject was
-- adopted into and which parent(s) adopted the child.
data AdoptionDetail
  = AdoptionDetail (GDRef Family) (Maybe Parent)
  deriving (Show)

-- | A calendar
data Calendar = Gregorian | Julian | Hebrew | French deriving (Show)

-- | A call number for citations
data CallNumber = CallNumber Text (Maybe MultimediaType) deriving (Show)

-- | The date a record was last changed
data ChangeDate = ChangeDate UTCTime (Maybe (GDRef Note)) deriving (Show)

-- | A character set and optional version (the version is unused so far as I am
-- aware)
data Charset = Charset Text (Maybe Text) deriving (Show)

-- | Metadata about a child link
data ChildLinkStatus = Challenged | Disproved | Proven deriving (Show)

-- | A date.  The format is day \/ month \/ year and months are numbered 1 to
-- 12 (or 1 to 13 for certain calendars).
data Date = Date Calendar (Maybe Word) (Maybe Word) Year deriving (Show)

-- | A range of dates
data DatePeriod
  = -- | A range of dates with a start date and optional end date.
    DateFrom Date (Maybe Date)
  | -- | A range of dates with only the end date specified.
    DateTo Date
  deriving (Show)

-- | A global location in longitude and latitude.
data MapCoord = MapCoord Longitude Latitude deriving (Show)

-- | A personal name.  The first field is the full name of the individual.  The
-- second field contains just the surname of the individual (or Nothing if
-- unspecified)
data Name = Name Text (Maybe Text) deriving (Show)

-- | Which parent?
data Parent = Husband | Wife | BothParents deriving (Show)

-- | How a child is associated with his\/her family.
data Pedigree = Adopted | ByBirth | Foster | Sealing deriving (Show)

-- | A phonetic transcription of a place name.
data PhoneticPlaceName = PhoneticPlaceName PhoneticType [Text] deriving (Show)

-- | The type of phonetic transcription.
data PhoneticType
  = -- | Japanese kana
    Kana
  | -- | Korean hanguel
    Hangul
  | -- | Something else
    PhoneticType Text
  deriving (Show)

-- | Privacy restrictions associated with a record.
data RestrictionNotice = Confidential | Locked | Privacy deriving (Show)

-- | A Roman transliteration of a place name.
data RomanPlaceName = RomanPlaceName RomanType [Text] deriving (Show)

-- | The type of Roman transliteration.
data RomanType
  = -- | Chinese Pinyin
    Pinyin
  | -- | Japanese Romaji
    Romaji
  | -- | Chinese Wade-Giles
    WadeGiles
  | -- | Something else
    RomanType Text
  deriving (Show)

-- | Sex.
data Sex = Male | Female | Undetermined deriving (Show)

-- | Description of a source.  The first field contains a description of the
-- source.  The second field contains sections of text from the source.
data SourceDescription = SourceDescription Text [Text] deriving (Show)

-- | Custom reference added by the creator of this file.  The format of this
-- reference is not standardized.
data UserReference = UserReference Text (Maybe Text) deriving (Show)

-- | A natural language.
newtype Language = Language Text deriving (Show)

-- | Latitude
newtype Latitude = Latitude Double deriving (Show)

-- | Longitude
newtype Longitude = Longitude Double deriving (Show)

-- | Assessment of the quality of a source.  A number from 0 to 3 with 0
-- indicating unreliable information and 3 being direct primary evidence.
newtype QualityAssessment = QualityAssessment Word deriving (Show)

-- | A year in some calendar.  May be negative.
newtype Year = Year Int deriving (Show)

-- | Automated Record ID.  From the GEDCOM standard:
--
-- "This number is intended to serve as a more sure means of identification of
-- a record for reconciling differences in data between two interfacing
-- systems."
newtype RIN = RIN Text deriving (Show)

-- | Part of the GEDCOM standard, but only for LDS use so far as I am aware.
newtype AFN = AFN Text deriving (Show)

-- | Part of the GEDCOM standard, but only for LDS use so far as I am aware.
newtype RFN = RFN Text deriving (Show)
