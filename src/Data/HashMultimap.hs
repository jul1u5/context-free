{-# LANGUAGE DerivingStrategies #-}

module Data.HashMultimap where

import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Monoid (Sum (..))
import GHC.Stack (HasCallStack)
import Prelude hiding (lookup, map)

newtype HashMultimap k v = HashMultimap {getHashMap :: HashMap k (HashSet v)}
  deriving newtype (Eq, Hashable)

instance (Show k, Show v) => Show (HashMultimap k v) where
  show m = "fromList " <> show (toList m)

-- * Creation

empty :: HashMultimap k v
empty = HashMultimap HashMap.empty

singleton :: (Hashable k, Hashable v) => k -> v -> HashMultimap k v
singleton k v = HashMultimap $ HashMap.singleton k (HashSet.singleton v)

-- * Basic interface

null :: HashMultimap k v -> Bool
null (HashMultimap m) = HashMap.null m

size :: HashMultimap k v -> Int
size (HashMultimap m) = getSum $ Prelude.foldMap (Sum . HashSet.size) m

member :: (Hashable k) => k -> HashMultimap k v -> Bool
member k (HashMultimap m) = HashMap.member k m

lookup :: (Hashable k) => k -> HashMultimap k v -> Maybe (HashSet v)
lookup k (HashMultimap m) = HashMap.lookup k m

(!?) :: (Hashable k) => HashMultimap k v -> k -> Maybe (HashSet v)
m !? k = lookup k m

findWithDefault :: (Hashable k) => HashSet v -> k -> HashMultimap k v -> HashSet v
findWithDefault d k (HashMultimap m) = HashMap.findWithDefault d k m

(!) :: (Hashable k, HasCallStack) => HashMultimap k v -> k -> HashSet v
(!) m k = case lookup k m of
  Just v -> v
  Nothing -> error "Data.HashMultimap.Internal.(!): key not found"

insert :: (Hashable k, Hashable v) => k -> v -> HashMultimap k v -> HashMultimap k v
insert k v (HashMultimap m) = HashMultimap $ HashMap.insertWith HashSet.union k (HashSet.singleton v) m

-- * Combine

union :: (Hashable k, Hashable v) => HashMultimap k v -> HashMultimap k v -> HashMultimap k v
union (HashMultimap m1) (HashMultimap m2) = HashMultimap $ HashMap.unionWith HashSet.union m1 m2

instance (Hashable k, Hashable v) => Semigroup (HashMultimap k v) where
  (<>) = union

instance (Hashable k, Hashable v) => Monoid (HashMultimap k v) where
  mempty = empty

-- * Compose

compose :: forall a b c. (Hashable b, Hashable c) => HashMultimap b c -> HashMultimap a b -> HashMultimap a c
compose (HashMultimap bc) (HashMultimap ab) = HashMultimap $ HashMap.mapMaybe f ab
  where
    f :: HashSet b -> Maybe (HashSet c)
    f bs = case catMaybes [bc HashMap.!? b | b <- HashSet.toList bs] of
      [] -> Nothing
      xs -> Just $ HashSet.unions xs

-- * Transformations

map :: (Hashable v2) => (v1 -> v2) -> HashMultimap k v1 -> HashMultimap k v2
map f = mapWithKey (const f)

mapKeys :: (Hashable k2) => (k1 -> k2) -> HashMultimap k1 v -> HashMultimap k2 v
mapKeys f (HashMultimap m) = HashMultimap $ HashMap.mapKeys f m

mapWithKey :: (Hashable v2) => (k -> v1 -> v2) -> HashMultimap k v1 -> HashMultimap k v2
mapWithKey f (HashMultimap m) = HashMultimap $ HashMap.mapWithKey (HashSet.map . f) m

traverse ::
  (Applicative f, Hashable v2) =>
  (v1 -> f v2) ->
  HashMultimap k v1 ->
  f (HashMultimap k v2)
traverse f = traverseWithKey (const f)

traverseWithKey ::
  (Applicative f, Hashable v2) =>
  (k -> v1 -> f v2) ->
  HashMultimap k v1 ->
  f (HashMultimap k v2)
traverseWithKey f (HashMultimap m) = HashMultimap <$> HashMap.traverseWithKey f' m
  where
    f' k vs = HashSet.fromList . HashMap.elems <$> HashMap.traverseWithKey (\v () -> f k v) (HashSet.toMap vs)

-- * Folds

foldMap :: (Monoid m) => (v -> m) -> HashMultimap k v -> m
foldMap f = foldMapWithKey (const f)

-- | /O(n)/. Fold the key\/value pairs in the map using the given monoid.
--
-- >>> foldMapWithKey (\k x -> show k ++ ":" ++ x) (fromList [(1, "a"), (1, "c"), (2, "b")])
-- "1:a1:c2:b"
foldMapWithKey :: (Monoid m) => (k -> v -> m) -> HashMultimap k v -> m
foldMapWithKey f (HashMultimap m) = HashMap.foldMapWithKey (Foldable.foldMap . f) m

-- * Filter

filter :: (v -> Bool) -> HashMultimap k v -> HashMultimap k v
filter f = filterWithKey (const f)

filterWithKey :: (k -> v -> Bool) -> HashMultimap k v -> HashMultimap k v
filterWithKey f (HashMultimap m) = HashMultimap $ HashMap.mapMaybeWithKey f' m
  where
    f' k vs = nothingOnEmpty $ HashMap.keysSet $ HashMap.filterWithKey (\v () -> f k v) $ HashSet.toMap vs

mapMaybe :: (Hashable v2) => (v1 -> Maybe v2) -> HashMultimap k v1 -> HashMultimap k v2
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: (Hashable v2) => (k -> v1 -> Maybe v2) -> HashMultimap k v1 -> HashMultimap k v2
mapMaybeWithKey f (HashMultimap m) = HashMultimap $ HashMap.mapMaybeWithKey f' m
  where
    f' k vs = nothingOnEmpty $ HashSet.fromList $ HashMap.elems $ HashMap.mapMaybeWithKey (\v () -> f k v) $ HashSet.toMap vs

-- * Conversions

keysSet :: HashMultimap k v -> HashSet k
keysSet (HashMultimap m) = HashMap.keysSet m

keys :: HashMultimap k v -> [k]
keys (HashMultimap m) = HashMap.keys m

elemsSet :: (Hashable v) => HashMultimap k v -> HashSet v
elemsSet (HashMultimap m) = HashSet.unions $ HashMap.elems m

elems :: (Hashable v) => HashMultimap k v -> [v]
elems (HashMultimap m) = HashSet.toList $ HashSet.unions $ HashMap.elems m

fromMap :: Hashable v => HashMap k v -> HashMultimap k v
fromMap = HashMultimap . HashMap.map HashSet.singleton

-- ** Lists

toList :: HashMultimap k v -> [(k, v)]
toList (HashMultimap m) =
  [(k, v) | (k, vs) <- HashMap.toList m, v <- HashSet.toList vs]

toGroupedList :: HashMultimap k v -> [(k, [v])]
toGroupedList (HashMultimap m) = List.map (fmap HashSet.toList) $ HashMap.toList m

fromList :: (Hashable k, Hashable v) => [(k, v)] -> HashMultimap k v
fromList = HashMultimap . HashMap.fromListWith HashSet.union . List.map (fmap HashSet.singleton)

fromGroupedList :: (Hashable k, Hashable v) => [(k, [v])] -> HashMultimap k v
fromGroupedList = HashMultimap . HashMap.fromListWith HashSet.union . List.map (fmap HashSet.fromList)

nothingOnEmpty :: HashSet a -> Maybe (HashSet a)
nothingOnEmpty set
  | HashSet.null set = Nothing
  | otherwise = Just set
