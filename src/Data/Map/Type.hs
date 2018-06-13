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
 
  -- * Difference
  , difference

  -- * Delete/Update
  , delete
  , adjust
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
newtype Map f = Map (M.Map Fingerprint (f Any))

type role Map nominal

infixl 9 !,!?,\\

(!) :: forall f a. Map f -> TypeRep a -> f a
(Map mf) ! rep = fromAny (mf M.! (typeRepFingerprint rep))
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
  Just x -> Just (fromAny x)
{-# INLINE lookup #-}

member :: TypeRep a -> Map f -> Bool
member rep (Map m) = M.member (typeRepFingerprint rep) m
{-# INLINE member #-}

notMember :: TypeRep a -> Map f -> Bool
notMember rep mf = not $ member rep mf
{-# INLINE notMember #-}

findWithDefault :: f a -> TypeRep a -> Map f -> f a
findWithDefault def rep mf = fromMaybe def (lookup rep mf)
{-# INLINE findWithDefault #-}

empty :: Map f
empty = Map M.empty
{-# INLINE empty #-}

singleton :: TypeRep a -> f a -> Map f
singleton rep v = Map (M.singleton (typeRepFingerprint rep) (toAny v))
{-# INLINE singleton #-}

insert :: TypeRep a -> f a -> Map f -> Map f
insert rep x (Map m) = Map (M.insert (typeRepFingerprint rep) (toAny x) m)
{-# INLINE insert #-}

insertWith :: (f a -> f a -> f a) -> TypeRep a -> f a -> Map f -> Map f
insertWith f rep x (Map m) = Map (M.insertWith (toAny2 f) (typeRepFingerprint rep) (toAny x) m)

difference :: Map f -> Map f -> Map f
difference (Map m1) (Map m2) = Map (M.difference m1 m2)
{-# INLINE difference #-}

delete :: TypeRep a -> Map f -> Map f
delete rep (Map m) = Map (M.delete (typeRepFingerprint rep) m)
{-# INLINE delete #-}

adjust :: (f a -> f a) -> TypeRep a -> Map f -> Map f
adjust f rep (Map m) = Map (M.adjust (toAny1 f) (typeRepFingerprint rep) m)
{-# INLINE adjust #-}

update :: (f a -> Maybe (f a)) -> TypeRep a -> Map f -> Map f
update f rep (Map m) = Map (M.update (aToMaybeAny f) (typeRepFingerprint rep) m)
{-# INLINE update #-}

aToMaybeAny :: forall f a. (f a -> Maybe (f a)) -> (f Any -> Maybe (f Any))
aToMaybeAny = unsafeCoerce
{-# INLINE aToMaybeAny #-}

toAny :: forall f a. f a -> f Any
toAny = unsafeCoerce
{-# INLINE toAny #-}

fromAny :: forall f a. f Any -> f a
fromAny = unsafeCoerce
{-# INLINE fromAny #-}

toAny1 :: forall f a. (f a -> f a) -> (f Any -> f Any)
toAny1 = unsafeCoerce
{-# INLINE toAny1 #-}

toAny2 :: forall f a. (f a -> f a -> f a) -> (f Any -> f Any -> f Any)
toAny2 f = unsafeCoerce f
{-# INLINE toAny2 #-}
