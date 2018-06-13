{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RoleAnnotations     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wall #-}

module Data.Map.Type
  (
  -- * Map type
  Map

  -- * Operators
  , (!), (!?), (\\)

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
  , insertWith
  , insertWithKey
  , insertLookupWithKey
 
  -- * Difference
  , difference

  -- * Delete/Update
  , delete
  , adjust
  , adjustWithKey
  , update
  ) where

import Prelude hiding (lookup, null)

import GHC.Exts
import Data.Maybe (fromMaybe)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Fingerprint (Fingerprint)
import Type.Reflection (TypeRep)
import Type.Reflection.Unsafe (typeRepFingerprint)
import qualified Data.Map.Strict as M

-- | Map from 'TypeRep' a to `f a`.
--   This type is /not/ safe if `a ~ Void`.
--
--   This type is /not/ safe for all GADTs:
--
--   If you match on `f Any` to get an `f Int`,
--   you have a proof that `Any ~ Int`.
newtype Map f = Map (M.Map Fingerprint (f Any))

type role Map nominal

infixl 9 !,!?,\\

(!) :: forall f a. Map f -> TypeRep a -> f a
(Map mf) ! rep = anyToA (mf M.! (typeRepFingerprint rep))
{-# INLINE (!) #-}

(!?) :: Map f -> TypeRep a -> Maybe (f a)
m !? typ = lookup typ m
{-# INLINE (!?) #-}

(\\) :: Map f -> Map f -> Map f
m1 \\ m2 = difference m1 m2
{-# INLINE (\\) #-}

null :: Map f -> Bool
null (Map m) = M.null m
{-# INLINE null #-}

size :: Map f -> Int
size (Map m) = M.size m
{-# INLINE size #-}

lookup :: TypeRep a -> Map f -> Maybe (f a)
lookup typ (Map m) = case M.lookup (typeRepFingerprint typ) m of
  Nothing -> Nothing
  Just x -> Just (anyToA x)
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
singleton rep v = Map (M.singleton (typeRepFingerprint rep) (aToAny v))
{-# INLINE singleton #-}

insert :: TypeRep a -> f a -> Map f -> Map f
insert rep x (Map m) = Map (M.insert (typeRepFingerprint rep) (aToAny x) m)
{-# INLINABLE insert #-}

insertWith :: (f a -> f a -> f a) -> TypeRep a -> f a -> Map f -> Map f
insertWith f rep x (Map m) = Map (M.insertWith (a3ToAny f) (typeRepFingerprint rep) (aToAny x) m)

insertWithKey :: (TypeRep a -> f a -> f a -> f a) -> TypeRep a -> f a -> Map f -> Map f
insertWithKey f rep x (Map m) = Map (M.insertWithKey (a3ToAnyWithFingerprint f) (typeRepFingerprint rep) (aToAny x) m)

insertLookupWithKey :: (TypeRep a -> f a -> f a -> f a) -> TypeRep a -> f a -> Map f -> (Maybe (f a), Map f)
insertLookupWithKey f rep x (Map m) = k (M.insertLookupWithKey (a3ToAnyWithFingerprint f) (typeRepFingerprint rep) (aToAny x) m)
  where
    k (p, y) = case p of
      Nothing -> (Nothing, Map y)
      Just q  -> (Just (anyToA q), Map y)

difference :: Map f -> Map f -> Map f
difference (Map m1) (Map m2) = Map (M.difference m1 m2)
{-# INLINABLE difference #-}

delete :: TypeRep a -> Map f -> Map f
delete rep (Map m) = Map (M.delete (typeRepFingerprint rep) m)
{-# INLINABLE delete #-}

adjust :: (f a -> f a) -> TypeRep a -> Map f -> Map f
adjust f rep (Map m) = Map (M.adjust (a2ToAny f) (typeRepFingerprint rep) m)
{-# INLINABLE adjust #-}

adjustWithKey :: (TypeRep a -> f a -> f a) -> TypeRep a -> Map f -> Map f
adjustWithKey f rep (Map m) = Map (M.adjustWithKey (a2ToAnyWithFingerprint f) (typeRepFingerprint rep) m) 
{-# INLINABLE adjustWithKey #-}

update :: (f a -> Maybe (f a)) -> TypeRep a -> Map f -> Map f
update f rep (Map m) = Map (M.update (aToMaybeAny f) (typeRepFingerprint rep) m)
{-# INLINABLE update #-}

aToMaybeAny :: forall f a. (f a -> Maybe (f a)) -> (f Any -> Maybe (f Any))
aToMaybeAny = unsafeCoerce
{-# INLINE aToMaybeAny #-}

aToAny :: forall f a. f a -> f Any
aToAny = unsafeCoerce
{-# INLINE aToAny #-}

anyToA :: forall f a. f Any -> f a
anyToA = unsafeCoerce
{-# INLINE anyToA #-}

a2ToAny :: forall f a. (f a -> f a) -> (f Any -> f Any)
a2ToAny = unsafeCoerce

a3ToAny :: forall f a. (f a -> f a -> f a) -> (f Any -> f Any -> f Any)
a3ToAny f = unsafeCoerce f
{-# INLINE a3ToAny #-}

a2ToAnyWithFingerprint :: (TypeRep a -> f a -> f a) -> (Fingerprint -> f Any -> f Any)
a2ToAnyWithFingerprint = unsafeCoerce
{-# INLINE a2ToAnyWithFingerprint #-}

a3ToAnyWithFingerprint :: (TypeRep a -> f a -> f a -> f a) -> (Fingerprint -> f Any -> f Any -> f Any)
a3ToAnyWithFingerprint = unsafeCoerce
{-# INLINE a3ToAnyWithFingerprint #-}
