module Data.Gedcom.Structure where

import Data.Time.Clock
import qualified Data.Text.All as T

-- The header

data Header = Header {
  headerSource :: HeaderSource,
  headerDestination :: Maybe T.Text,
  headerDate :: Maybe UTCTime,
  headerSubmitter :: Submitter,
  headerSubmission :: Maybe Submission,
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
  familyHusband :: Maybe Individual,
  familyWife :: Maybe Individual,
  familyChildren :: [Individual],
  familyTotalChildren :: Maybe Int,
  familySubitter :: [Submitter],
  familyUserReference :: [UserReference],
  familyRIN :: Maybe RIN,
  familyChangeDate :: Maybe ChangeDate,
  familyNote :: [Note],
  familySourceCitation :: [SourceCitation],
  familyMultimedia :: [Multimedia]
} deriving Show

data Individual = Individual {
  individualRestrictionNotice :: Maybe RestrictionNotice,
  individualName :: Maybe PersonalName,
  individualSex :: Maybe Sex,
  individualAttribute :: [IndividualAttribute],
  individualSubmitter :: [Submitter],
  individualAlias :: [Individual],
  individualAncestorInterest :: [Submitter],
  individualDescendantInterest :: [Submitter],
  individualRFN :: Maybe RFN,
  individualAFN :: Maybe AFN,
  individualUserReference :: [UserReference],
  individualRIN :: Maybe RIN,
  individualChangeDate :: Maybe ChangeDate,
  individualNote :: [Note],
  individualSourceCitation :: [SourceCitation],
  individualMultimedia :: [Multimedia]
} deriving Show

data Multimedia = Multimedia {
  multimediaFile :: [MultimediaFile],
  multimediaTitl :: Maybe T.Text,
  multimediaUserReference :: [UserReference],
  multimediaRIN :: Maybe RIN,
  multimediaNote :: [Note],
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
  repositoryNote :: [Note],
  repositoryUserReference :: [UserReference],
  repositoryRIN :: Maybe RIN,
  repositoryChangeDate :: Maybe ChangeDate
} deriving Show

data Source = Source {
  sourceAuthor :: Maybe T.Text,
  sourceTitle :: Maybe T.Text,
  sourceShortTitle :: Maybe T.Text,
  sourcePublicationFacts :: Maybe T.Text,
  sourceText :: Maybe T.Text,
  sourceRepositoryCitations :: [RepositoryCitation],
  sourceUserReference :: Maybe UserReference,
  sourceRIN :: Maybe RIN,
  sourceChangeDate :: Maybe ChangeDate,
  sourceNote :: [Note],
  sourceMultimedia :: [Multimedia]
} deriving Show

data Submission = Submission {
  submissionSubmitter :: Maybe Submitter,
  submissionFamilyFile :: Maybe T.Text,
  submissionTempleCode :: Maybe T.Text,
  submissionAncestorGenerations :: Maybe Int,
  submissionDescendentGenerations :: Maybe Int,
  submissionOrdinanceProcessing :: Maybe Bool,
  submissionRIN :: Maybe RIN,
  submissionNote :: [Note],
  submissionChangeDate :: Maybe ChangeDate
} deriving Show

data Submitter = Submitter {
  submitterName :: T.Text,
  submitterAddress :: Maybe Address,
  submitterMedia :: Maybe Multimedia,
  submitterLang :: [Language],
  submitterRFN :: Maybe RFN,
  submitterRIN :: Maybe RIN,
  submitterNote :: [Note],
  submitterChangeDate :: Maybe ChangeDate
} deriving Show

-- Substructures

data PersonalName = PersonalName {
  personalNameName :: T.Text,
  personalNameType :: NameType,
  personalNamePieces :: PersonalNamePieces,
  personalNamePhonetic :: [PhoneticName],
  personalNameRoman :: [RomanizedName]
} deriving Show

data PhoneticName = PhoneticName {
  phoneticName :: T.Text,
  phoneticType :: PhoneticType,
  phoneticPieces :: PersonalNamePieces
} deriving Show

data RomanizedName = RomanizedName {
  romanziedName :: T.Text,
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
  namePieceNameNote :: Maybe Note
} deriving Show

data IndividualAttribute =
    Caste T.Text
  | PhysicalDescription T.Text
  | Education T.Text
  | NationalID T.Text
  | NationalOrigin T.Text
  | NChildren Int
  | NMarriages T.Text
  | Occupation T.Text
  | Possessions T.Text
  | Religion T.Text
  | ResidesAt
  | SocialSecurity T.Text
  | Title T.Text
  | Fact T.Text deriving Show

data SourceCitation = SourceCitation {
  citeSource :: Either SourceDescription Source,
  citePage :: Maybe T.Text,
  citeMultimedia :: [Multimedia],
  citeNote :: [Note],
  citeQuality :: Maybe QualityAssessment
} deriving Show

data RepositoryCitation = RepositoryCitation {
  repoCiteRepository :: Maybe Repository,
  repoCiteNote :: [Note],
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
  AKA | Birth | Immigrant | Maiden | Married | NameType T.Text
  deriving Show

data ChangeDate = ChangeDate UTCTime (Maybe Note) deriving Show
data Charset = Charset T.Text (Maybe T.Text) deriving Show
data RestrictionNotice = Confidential | Locked | Privacy deriving Show
data Sex = Male | Female | Undetermined deriving Show
data UserReference = UserReference T.Text (Maybe T.Text) deriving Show
data PhoneticType = Kana | Hangul | PhoneticType T.Text deriving Show
data RomanType = Pinyin | Romaji | WadeGiles | RomanType T.Text deriving Show
data SourceDescription = SourceDescription T.Text [T.Text] deriving Show
data CallNumber = CallNumber T.Text MultimediaType deriving Show

newtype RIN = RIN T.Text deriving Show
newtype RFN = RFN T.Text deriving Show
newtype AFN = AFN T.Text deriving Show
newtype Language = Language T.Text deriving Show
newtype QualityAssessment = QualityAssessment Int deriving Show

