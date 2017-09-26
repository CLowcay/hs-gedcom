{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Gedcom.ParseMonads where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Dynamic
import Data.Either
import Data.Foldable
import Data.Gedcom.Common
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text.All as T

type StructureParser a = GDTree -> StructureMonad (Either GDTree a)

-- The MultiMonad

newtype MultiMonad a =
  MultiMonad (ExceptT GDError (StateT [GDTree] StructureMonad) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

runMultiMonad :: [GDTree] -> MultiMonad a -> StructureMonad a
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
  (mr, leftover) <-
    lift$ lift$ foldrM (\v (r, rest) ->
      if isJust r then return (r, v:rest)
      else pick v rest.toMaybe <$> p v) (Nothing, []) ls
  lift$ put leftover
  return mr
  where
    toMaybe (Left _) = Nothing
    toMaybe (Right v) = Just v
    pick v rest Nothing = (Nothing, v:rest)
    pick _ rest x = (x, rest)

parseRequired :: GDTag -> StructureParser a -> MultiMonad a
parseRequired tag p = do
  r <- parseOptional p
  case r of
    Just v -> return v
    Nothing -> throwError.TagError$
      "Could not find required " <> (T.show tag) <> " tag"

-- The StructureMonad

newtype StructureMonad a =
  StructureMonad (ExceptT GDError (State (M.Map GDXRefID Dynamic)) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

addReference :: Typeable a => GDXRefID -> a -> StructureMonad ()
addReference thisID value = StructureMonad$ do
  alreadySeen <- M.member thisID <$> lift get
  when alreadySeen$
    throwError.DuplicateRef$ "Duplicate definition of " <> (T.show thisID)
  lift.modify$ M.insert thisID (toDyn value)
  return ()

runStructure :: StructureMonad a -> (Either GDError a, M.Map GDXRefID Dynamic)
runStructure (StructureMonad m) = (flip runState) M.empty . runExceptT$ m

