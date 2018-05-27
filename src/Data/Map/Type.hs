{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Map.Type
  ( Map
  , empty
  , lookup
  , insert
  ) where

import Prelude hiding (lookup)

import GHC.Exts
import Unsafe.Coerce (unsafeCoerce)
import GHC.Fingerprint (Fingerprint)
import Type.Reflection (TypeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)
import qualified Data.Map.Strict as M

newtype Map f = Map (M.Map Fingerprint (f Any))

empty :: Map f
empty = Map M.empty

lookup :: TypeRep a -> Map f -> Maybe (f a)
lookup typ (Map m) = case M.lookup (typeRepFingerprint typ) m of
  Nothing -> Nothing
  Just x -> Just (unsafeCoerce x)

insert :: forall f a. TypeRep a -> f a -> Map f -> Map f
insert rep x (Map m) = Map (M.insert (typeRepFingerprint rep) (unsafeCoerce x) m)

