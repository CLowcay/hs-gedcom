{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.Gedcom.Parser where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Dynamic
import Data.Either
import Data.Foldable
import Data.Gedcom.Internal.Common
import Data.Gedcom.LineParser
import Data.Gedcom.Structure
import Data.Maybe
import Data.Monoid
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Map.Lazy as M
import qualified Data.Text.All as T
import Text.Megaparsec

newtype GDStructure = GDStructure Dynamic
data GDError = XRefError T.Text deriving Show

parseReferredStructure :: GDTree -> LookupMonad GDStructure
parseReferredStructure t@(GDTree (GDLine _ _ tag _) _) =
  GDStructure <$> parser t
  where
    parser = case tag of
      gd@(GDTag "SUBM") -> makeDynamic gd$ parseSubmitter
      gd@(GDTag "SUBN") -> makeDynamic gd$ parseSubmission
      gd@(GDTag "INDI") -> undefined
      gd@(GDTag "NOTE") -> makeDynamic gd$ parseNote
      gd@(GDTag "REPO") -> undefined
      gd@(GDTag "SOUR") -> undefined
      gd@(GDTag "FAM" ) -> undefined
      gd@(GDTag "OBJE") -> makeDynamic gd$ parseMultimediaRecord
      _ -> \_ -> throwError.XRefError$ "Invalid reference to " <> (T.show tag)

    makeDynamic :: Typeable a =>
      GDTag -> StructureParser a -> (GDTree -> LookupMonad Dynamic)
    makeDynamic tag' = fmap$
      (fmap toDyn).
      ((either
        (const.throwError.XRefError$ "Required " <> (T.show tag') <> " tag")
        pure) =<<)

type StructureParser a = GDTree -> LookupMonad (Either GDTree a)
type TagHandler b a = (Either b T.Text, [GDTree]) -> LookupMonad a
type LinkHandler b a = (b, [GDTree]) -> LookupMonad a
type NoLinkTagHandler a = (T.Text, [GDTree]) -> LookupMonad a

noLink :: NoLinkTagHandler a -> TagHandler a a
noLink _ (Left l, _) = return l
noLink nl (Right t, children) = nl (t, children)

noText :: forall a b. LinkHandler b a -> TagHandler b a
noText lh (Left l, children) = lh (l, children)
noText _ (Right _, _) = throwError.XRefError$
  "Required link to " <> (T.show$ (Proxy :: Proxy b))
  <> " when parsing " <> (T.show$ (Proxy :: Proxy a))

-- Header
parseHeader :: StructureParser Header
parseHeader = parseTag (GDTag "HEAD")$ \(_, children) ->
  runMultiMonad children$ Header
    <$> parseRequired (GDTag "SOUR") parseHeaderSource
    <*> parseOptional (parseTextTag (GDTag "DEST"))
    <*> parseOptional parseDateTime
    <*> parseRequired (GDTag "SUBM") parseSubmitter
    <*> parseOptional parseSubmission
    <*> parseOptional parseFile
    <*> parseOptional parseCopyright
    <*> parseRequired (GDTag "GEDC") parseGedcomFormat
    <*> parseRequired (GDTag "CHAR") parseCharset
    <*> parseOptional parseLanguage
    <*> parseOptional parsePlaceForm
    <*> parseOptional (parseTextTag (GDTag "NOTE"))

parseHeaderSource :: StructureParser HeaderSource
parseHeaderSource = parseTag (GDTag "SOUR")$ \(sid, children) ->
  runMultiMonad children$ HeaderSource sid
    <$> parseOptional parseVersion
    <*> parseOptional parseName
    <*> parseOptional parseCorp
    <*> parseOptional parseHeaderSourceData

parseCorp :: StructureParser Corp
parseCorp = parseTag (GDTag "COPR")$ \(name, children) ->
  runMultiMonad children$ Corp name
    <$> (parseContactDetails >>= (parseOptional.parseAddress))

parseHeaderSourceData :: StructureParser HeaderSourceData
parseHeaderSourceData = parseTag (GDTag "DATA")$ \(name, children) ->
  runMultiMonad children$ HeaderSourceData name
    <$> parseOptional parseDate
    <*> parseOptional parseCopyright

-- Records

parseNote :: StructureParser Note
parseNote = parseTag (GDTag "NOTE")$ \(t, children) ->
  runMultiMonad children$ Note t
    <$> parseMulti parseUserReference
    <*> parseOptional parseRIN
    <*> parseMulti parseSourceCitation
    <*> parseOptional parseChangeDate

parseSubmitter :: StructureParser Submitter
parseSubmitter = parseTag (GDTag "SUBM")$ \(_, children) ->
  runMultiMonad children$ Submitter
    <$> parseRequired (GDTag "NAME") parseName
    <*> (parseContactDetails >>= (parseOptional.parseAddress))
    <*> parseOptional parseMultimedia
    <*> parseMulti parseLanguage
    <*> parseOptional parseRFN
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseOptional parseChangeDate

parseSubmission :: StructureParser Submission
parseSubmission = parseTag (GDTag "SUBN")$ \(_, children) ->
  runMultiMonad children$ Submission
    <$> parseOptional parseSubmitter
    <*> parseOptional (parseTextTag (GDTag "FAMF"))
    <*> parseOptional (parseTextTag (GDTag "TEMP"))
    <*> parseOptional (parseIntTag (GDTag "ANCE"))
    <*> parseOptional (parseIntTag (GDTag "DESC"))
    <*> parseOptional (parseBoolTag (GDTag "ORDI"))
    <*> parseOptional parseRIN
    <*> parseMulti parseNote
    <*> parseOptional parseChangeDate

-- Substructures

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
        <$> (Left . SourceDescription description <$>
          parseMulti (parseTextTag (GDTag "TEXT")))
        <*> pure Nothing
        <*> parseMulti parseMultimedia
        <*> parseMulti parseNote
        <*> parseOptional parseQuality

parseGedcomFormat :: StructureParser GedcomFormat
parseGedcomFormat = parseTag (GDTag "GEDC")$ \(_, children) ->
  runMultiMonad children$ GedcomFormat
    <$> parseRequired (GDTag "VERS") parseVersion
    <*> parseRequired (GDTag "FORM") parseGedcomForm

parseGedcomForm :: StructureParser GedcomForm
parseGedcomForm = parseTag (GDTag "FORM")$ \(t, _) ->
  return$ if T.toUpper t == "LINEAGE-LINKED"
    then GedcomLineageLinked else GedcomUnsupported t

parseCharset :: StructureParser Charset
parseCharset = parseTag (GDTag "CHAR")$ \(cs, children) ->
  runMultiMonad children$ Charset cs <$> parseOptional parseVersion
parseChangeDate :: StructureParser ChangeDate
parseChangeDate = parseTag (GDTag "CHAN")$ \(_, children) ->
  runMultiMonad children$ ChangeDate
    <$> parseRequired (GDTag "DATE") parseDateTime
    <*> parseOptional parseNote

parseContactDetails :: MultiMonad ContactDetails
parseContactDetails = ContactDetails
  <$> parseMulti (parseTextTag (GDTag "PHON"))
  <*> parseMulti (parseTextTag (GDTag "EMAIL"))
  <*> parseMulti (parseTextTag (GDTag "FAX"))
  <*> parseMulti (parseTextTag (GDTag "WWW"))

parseAddress :: ContactDetails -> StructureParser Address
parseAddress contacts = parseTag (GDTag "ADDR")$ \(addr, children) ->
  runMultiMonad children$ Address addr
    <$> parseOptional (parseTextTag (GDTag "CITY"))
    <*> parseOptional (parseTextTag (GDTag "STAE"))
    <*> parseOptional (parseTextTag (GDTag "POS"))
    <*> parseOptional (parseTextTag (GDTag "CTRY"))
    <*> pure contacts

-- Values

parseDate :: StructureParser UTCTime
parseDate = parseTag (GDTag "DATE")$ \(date, _) ->
  case parseMaybe dateExact date of
    Nothing -> throwError.XRefError$ "Bad date \"" <> (T.show date) <> "\""
    Just (d, m, y) ->
      return$ UTCTime (fromGregorian (fromIntegral y) m d) (secondsToDiffTime 0)

parseDateTime :: StructureParser UTCTime
parseDateTime = parseTag (GDTag "DATE")$ \(date, children) -> do
  mtime <- runMultiMonad children$ parseOptional (parseTextTag (GDTag "TIME"))

  dt <- case mtime of
    Nothing -> return$ secondsToDiffTime 0
    Just t -> case parseMaybe timeValue t of
      Nothing -> throwError.XRefError$ "Bad time \"" <> (T.show t) <> "\""
      Just Nothing -> return$ secondsToDiffTime 0
      Just (Just time) -> return$ timeToPicos time

  case parseMaybe dateExact date of
    Nothing -> throwError.XRefError$ "Bad date \"" <> (T.show date) <> "\""
    Just (d, m, y) -> return$ UTCTime (fromGregorian (fromIntegral y) m d) dt

parseMultimediaRecord :: StructureParser Multimedia
parseMultimediaRecord = parseMultimediaRaw (GDTag "TYPE")

parseMultimedia :: StructureParser Multimedia
parseMultimedia = parseMultimediaRaw (GDTag "MEDI")

parseMultimediaRaw :: GDTag -> StructureParser Multimedia
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
parseMultimediaFile typeTag mf = parseTag (GDTag "FILE")$ \(name, children) -> do
  (mc, title) <- runMultiMonad children$ (,)
    <$> parseOptional (parseMultimediaFormat typeTag)
    <*> parseOptional (parseTextTag (GDTag "TITL"))
  case mc <|> mf of
    Nothing -> throwError.XRefError$ "Missing FORM tag for file format"
    Just c -> return$ MultimediaFile name c title

parseMultimediaFormat :: GDTag -> StructureParser MultimediaFormat
parseMultimediaFormat tag = parseTag (GDTag "FORM")$ \(v, children) -> do
  runMultiMonad children$ MultimediaFormat
    (fromMaybe (MF_OTHER v)$ parseMaybe parser v)
    <$> parseOptional (parseMultimediaType tag)

  where
    parser :: Parser MultimediaFileFormat
    parser =     (MF_BMP <$ string' "audio")
                 <|> (MF_GIF <$ string' "book")
                 <|> (MF_JPG <$ string' "card")
                 <|> (MF_OLE <$ string' "electronic")
                 <|> (MF_PCX <$ string' "fiche")
                 <|> (MF_TIF <$ string' "film")
                 <|> (MF_WAV <$ string' "magazine")

parseMultimediaType :: GDTag -> StructureParser MultimediaType
parseMultimediaType tag = parseTag tag$ \(v, _) ->
  return.fromMaybe (MT_OTHER v)$ parseMaybe parser v
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

parsePlaceForm :: StructureParser [T.Text]
parsePlaceForm = parseTag (GDTag "PLAC")$ \(_, children) ->
  runMultiMonad children$ fromMaybe [].(fmap$ T.splitOn ",") <$>
    parseOptional (parseTextTag (GDTag "FORM"))

parseUserReference :: StructureParser UserReference
parseUserReference = parseTag (GDTag "REFN")$ \(i, children) ->
  runMultiMonad children$ UserReference i
    <$> parseOptional (parseTextTag (GDTag "TYPE"))

parseVersion :: StructureParser T.Text
parseVersion = parseTextTag (GDTag "VERS")

parseName :: StructureParser T.Text
parseName = parseTextTag (GDTag "NAME")

parseCopyright :: StructureParser T.Text
parseCopyright = parseTextTag (GDTag "COPR")

parseFile :: StructureParser FilePath
parseFile = parseTag (GDTag "FILE") (return. T.unpack . fst)

parseRFN :: StructureParser RFN
parseRFN = parseTag (GDTag "RFN") (return. RFN . fst)

parseRIN :: StructureParser RIN
parseRIN = parseTag (GDTag "RIN") (return. RIN . fst)

parseLanguage :: StructureParser Language
parseLanguage = parseTag (GDTag "LANG") (return. Language . fst)

parseQuality :: StructureParser QualityAssessment
parseQuality = (fmap.fmap.fmap) (QualityAssessment) $ parseIntTag (GDTag "QUAY")

-- General parsers

parseBoolTag :: GDTag -> StructureParser Bool
parseBoolTag tag = parseTag tag$ \(v, _) ->
  case parseMaybe ynParser v of
    Nothing -> throwError.XRefError$ "Expected boolean, saw " <> (T.show v)
    Just yn -> return yn
  where
    ynParser :: Parser Bool
    ynParser = (True <$ string' "yes") <|> (False <$ string' "no")

parseIntTag :: GDTag -> StructureParser Int
parseIntTag tag = parseTag tag$ \(v, _) ->
  case parseMaybe parser v of
    Nothing -> throwError.XRefError$ "Expected number, saw " <> (T.show v)
    Just n -> return . read $ n
  where
    parser :: Parser String
    parser = many digitChar

parseTextTag :: GDTag -> StructureParser T.Text
parseTextTag tag = parseTag tag (return.fst)

parseTag :: Typeable a => GDTag -> NoLinkTagHandler a -> StructureParser a
parseTag tag nl = parseTagFull tag$ noLink nl

parseLinkTag :: (Typeable a, Typeable b) =>
  GDTag -> LinkHandler b a -> StructureParser a
parseLinkTag tag lh = parseTagFull tag$ noText lh

parseTagFull :: forall a b. (Typeable a, Typeable b) =>
  GDTag -> TagHandler b a -> StructureParser a
parseTagFull tag handler t@(GDTree (GDLine _ _ tag' v) children) =
  if tag /= tag' then return$ Left t else case v of
    Nothing -> Right <$> (handler.(first Right)$ parseCont "" children)
    Just (GDXRefIDV xref) -> do
      GDStructure xdv <- crossRef xref
      case fromDynamic xdv of
        Nothing -> do
          throwError.XRefError$ "Expected " <>
            (T.show$ typeOf (Proxy :: Proxy b))
            <> " but found a " <> (T.show$ typeOf xdv)
        Just xv -> Right <$> (handler (Left xv, children))
    Just (GDLineItemV (GDLineItem text)) ->
      Right <$> (handler.(first Right)$ parseCont text children)

parseCont :: T.Text -> [GDTree] -> (T.Text, [GDTree])
parseCont l1 children =
    bimap assemble (fmap snd).span (isJust.fst)$
      zipWith (\l r -> (cont l <|> conc l, r)) children children
  where
    assemble = T.concat . (l1:) . catMaybes . fmap fst
    cont (GDTree (GDLine _ _ (GDTag "CONT") (Just (GDLineItemV (GDLineItem l)))) _) =
      Just$ "\r\n" <> l
    cont _ = Nothing
    conc (GDTree (GDLine _ _ (GDTag "CONC") (Just (GDLineItemV (GDLineItem l)))) _) =
      Just$ trim l
    conc _ = Nothing

-- The MultiMonad

newtype MultiMonad a =
  MultiMonad (ExceptT GDError (StateT [GDTree] LookupMonad) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

runMultiMonad :: [GDTree] -> MultiMonad a -> LookupMonad a
runMultiMonad children (MultiMonad m) =
  ((flip evalStateT) children.runExceptT$ m) >>= rethrowError
  where rethrowError x = case x of
                           Left e -> throwError e
                           Right v -> return v

parseMulti :: StructureParser a -> MultiMonad [a]
parseMulti p = MultiMonad$ do
  ls <- lift$ get
  (others, vs) <- lift$ lift$ partitionEithers <$> p `traverse` ls
  lift$ put others
  return vs

parseOptional :: StructureParser a -> MultiMonad (Maybe a)
parseOptional p = MultiMonad$ do
  ls <- lift$ get
  (mr, leftover) <- lift$ lift$ foldrM (\v (r, rest) ->
    if isJust r then return (r, v:rest)
    else (, rest).toMaybe <$> p v) (Nothing, []) ls
  lift$ put leftover
  return mr
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right v) = Just v

parseRequired :: GDTag -> StructureParser a -> MultiMonad a
parseRequired tag p = do
  r <- parseOptional p
  case r of
    Just v -> return v
    Nothing -> throwError.XRefError$
      "Could not find required " <> (T.show tag) <> " tag"

-- The LookupMonad

newtype LookupMonad a =
  LookupMonad (ExceptT GDError (Reader (M.Map GDXRefID GDStructure)) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

runLookup :: M.Map GDXRefID GDStructure -> LookupMonad a -> Either GDError a
runLookup lm (LookupMonad m) = (flip runReader) lm.runExceptT$ m

crossRef :: GDXRefID -> LookupMonad GDStructure
crossRef xrid = LookupMonad$ do
  r <- M.lookup xrid <$> (lift ask)
  case r of
    Nothing -> throwError$ XRefError (T.show xrid)
    Just v -> return v

tieTheKnot ::
  M.Map GDXRefID (LookupMonad GDStructure) ->
  Either GDError (M.Map GDXRefID GDStructure)
tieTheKnot mt = mdo
  t <- mapM (runLookup t) mt
  return t

computeXrefTable :: GDRoot -> M.Map GDXRefID (LookupMonad GDStructure)
computeXrefTable (GDRoot trees) =
  foldr M.union M.empty . fmap computeTable$ trees
  where 
    computeTable t@(GDTree line children) =
      let ct = foldr M.union M.empty . fmap computeTable$ children
      in case line of
        GDLine _ Nothing _ _ -> ct
        GDLine _ (Just xrid) _ _ -> M.insert xrid (parseReferredStructure t) ct

