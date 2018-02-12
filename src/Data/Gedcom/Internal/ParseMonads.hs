{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Data.Gedcom.ParseMonads
Description: Monads for parsing GEDCOM records
Copyright: (c) Callum Lowcay, 2017
License: BSD3
Maintainer: cwslowcay@gmail.com
Stability: experimental
Portability: GHC

This module contains monads and utility functions for extracting GEDCOM records
from the raw syntax tree.

-}
module Data.Gedcom.Internal.ParseMonads (
  StructureParser,
  MultiMonad,
  runMultiMonad,
  parseMulti,
  parseOptional,
  parseRequired,
  StructureMonad,
  addReference,
  runStructure
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Dynamic
import Data.Either
import Data.Foldable
import Data.Gedcom.Internal.CoreTypes
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Text.All as T

-- | A parser that extracts a GEDCOM structure from a GEDCOM subtree.
type StructureParser a =
     GDTree                               -- ^ The  subtree to parse.
  -> StructureMonad (Either GDTree a)     -- ^ Either parsed structure, or the subtree itself if the subtree doesn't contain the expected GEDCOM structure.

-- | A Monad for parsing GEDCOM structures out of a list of GEDCOM subtrees.
newtype MultiMonad a =
  MultiMonad (ExceptT GDError (StateT [GDTree] StructureMonad) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

-- | Run a 'MultiMonad' into a 'StructureMonad'.
runMultiMonad ::
     [GDTree]         -- ^ The subtrees to parse the structure from.
  -> MultiMonad a     -- ^ The MultiMonad that does the parsing.
  -> StructureMonad a
runMultiMonad children (MultiMonad m) =
  (flip evalStateT children.runExceptT$ m) >>= rethrowError
  where rethrowError x = case x of
                           Left e -> throwError e
                           Right v -> return v

-- | Parse multiple instances of a structure
parseMulti :: StructureParser a -> MultiMonad [a]
parseMulti p = MultiMonad$ do
  ls <- lift get
  (others, vs) <- lift$ lift$ partitionEithers <$> p `traverse` ls
  lift$ put others
  return vs

-- | Parse an optional instance of a structure
parseOptional :: StructureParser a -> MultiMonad (Maybe a)
parseOptional p = MultiMonad$ do
  ls <- lift get
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

-- | Parse a required instance of a structure
parseRequired ::
     GDTag       -- ^ The tag that identifies the required structure.
  -> StructureParser a
  -> MultiMonad a
parseRequired tag p = do
  r <- parseOptional p
  case r of
    Just v -> return v
    Nothing -> throwError.TagError$
      "Could not find required " <> T.show tag <> " tag"

-- | A monad for parsing an instance of a GEDCOM structure from a GEDCOM
-- subtree.
newtype StructureMonad a =
  StructureMonad (ExceptT GDError (State (M.Map GDXRefID Dynamic)) a)
  deriving (Monad, Functor, Applicative, MonadError GDError)

-- | Add a reference to the cross reference table.
addReference :: Typeable a
  => GDXRefID            -- ^ The cross reference to add.
  -> a                   -- ^ The value the reference will resolve to.
  -> StructureMonad ()
addReference thisID value = StructureMonad$ do
  alreadySeen <- M.member thisID <$> lift get
  when alreadySeen$
    throwError.DuplicateRef$ "Duplicate definition of " <> T.show thisID
  lift.modify$ M.insert thisID (toDyn value)
  return ()

-- | Run a 'StructureMonad', returning either an error or a value, and the
-- cross reference table.
runStructure :: StructureMonad a -> (Either GDError a, M.Map GDXRefID Dynamic)
runStructure (StructureMonad m) = flip runState M.empty . runExceptT$ m

