{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Map.Type
  (
  -- * Map type
  Map

  -- * Operators
  , (!?), (\\)

  -- * Query
  , null
  , size
  , member
  , notMember
  , findWithDefault
 
  -- * Construction
  , empty
  , singleton

  -- * Insertion
  , insert
  
  ) where

import Prelude hiding (lookup, null)

import GHC.Exts
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Fingerprint (Fingerprint)
import Type.Reflection (TypeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)
import qualified Data.Map.Strict as M

newtype Map f = Map (M.Map Fingerprint (f Any))

type role Map nominal

infixl 9 !?,\\

(!?) :: TypeRep a -> Map f -> Maybe (f a)
typ !? m = lookup typ m
{-# INLINABLE (!?) #-}

(\\) :: Map f -> Map f -> Map f
m1 \\ m2 = difference m1 m2

null :: Map f -> Bool
null (Map m) = M.null m
{-# INLINE null #-}

size :: Map f -> Int
size (Map m) = M.size m
{-# INLINE size #-}

lookup :: TypeRep a -> Map f -> Maybe (f a)
lookup typ (Map m) = case M.lookup (typeRepFingerprint typ) m of
  Nothing -> Nothing
  Just x -> Just (unsafeCoerce x)
{-# INLINABLE lookup #-}

member :: TypeRep a -> Map f -> Bool
member rep (Map m) = M.member (typeRepFingerprint rep) m
{-# INLINABLE member #-}

notMember :: TypeRep a -> Map f -> Bool
notMember rep mf = not $ member rep mf
{-# INLINABLE notMember #-}

findWithDefault :: f a -> TypeRep a -> Map f -> f a
findWithDefault def rep mf = fromMaybe def (lookup rep mf)
{-# INLINABLE findWithDefault #-}

empty :: Map f
empty = Map M.empty
{-# INLINE empty #-}

singleton :: TypeRep a -> f a -> Map f
singleton rep v = Map (M.singleton (typeRepFingerprint rep) (unsafeCoerce v))
{-# INLINE singleton #-}

insert :: forall f a. TypeRep a -> f a -> Map f -> Map f
insert rep x (Map m) = Map (M.insert (typeRepFingerprint rep) (unsafeCoerce x) m)
{-# INLINABLE insert #-}

difference :: Map f -> Map f -> Map f
difference (Map m1) (Map m2) = Map (M.difference m1 m2)
{-# INLINABLE difference #-}
