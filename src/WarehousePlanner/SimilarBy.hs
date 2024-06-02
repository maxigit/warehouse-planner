{-# LANGUAGE PatternSynonyms #-}
module WarehousePlanner.SimilarBy
( SimilarBy
, unSimilar
, similarKey
, pattern SimilarBy
, groupSimilar
, groupSimilars
, sortSimilarOn
, dropSimilar
, splitSimilar
, unsplitSimilar
)
where
import Prelude
import Data.List(groupBy, sortOn)
import Data.Function(on)
import Data.Map qualified as Map
import Data.Map (Map)

data SimilarBy k a = SimilarByPrivate k a [a] deriving Show


{-# COMPLETE SimilarBy #-}
pattern SimilarBy :: k -> a -> [a] -> SimilarBy k a
pattern SimilarBy k x xs <- SimilarByPrivate k x xs

unSimilar :: SimilarBy k a -> [a]
unSimilar (SimilarByPrivate _ x xs) = x:xs

similarKey :: SimilarBy k a -> k
similarKey (SimilarByPrivate k _ _ ) = k

-- | Similar to prelude `group`, ie doesn't sort
groupSimilar :: (Eq k) => (a -> k) -> [a] ->  [SimilarBy k a]
groupSimilar k  xs0 = let
  groups = groupBy ((==) `on` k) xs0
  mkSimilar (x:xs) = SimilarByPrivate (k x) x xs
  mkSimilar [] = error "Should not happend because groupBy doesn't create empty group"
  in map mkSimilar groups

sortSimilarOn :: (Ord b) => (a -> b) -> SimilarBy k a -> SimilarBy k a
sortSimilarOn fn  (SimilarByPrivate k x0 xs0) = case sortOn fn (x0:xs0) of
  [] -> error "Shouldn't happend: initial ilst is not empty"
  (x:xs) -> SimilarByPrivate k x xs

dropSimilar :: Int -> SimilarBy k a -> Maybe (SimilarBy k a)
dropSimilar n = snd . splitSimilar n

splitSimilar :: Int -> SimilarBy k a -> (Maybe (SimilarBy k a), Maybe (SimilarBy k a))
splitSimilar n (SimilarByPrivate k x xs) = let 
  (taken, dropped) = splitAt n (x:xs)
  in (mkSimilar taken, mkSimilar dropped)
  where mkSimilar [] = Nothing
        mkSimilar (x:xs) = Just $ SimilarByPrivate k x xs
        
unsplitSimilar :: Eq k => SimilarBy k a -> SimilarBy k a -> Maybe (SimilarBy k a)
unsplitSimilar (SimilarByPrivate k x xs) (SimilarBy k' x' xs') = 
  if k == k'
  then Just $ SimilarByPrivate k x (xs ++ x':xs')
  else Nothing 
-- | Group by k but separate groups with the same key
-- >>> groupTo [(1, a), (1, b), (2,  c), (1, d)] = {1 : [[a b ], [d], 2: [[c]] }
groupSimilars :: Ord k => (a -> k) -> [a] -> Map k (SimilarBy k [a])
groupSimilars key = 
  Map.mapWithKey (\k sims_ -> let sim:sims = reverse sims_
                                  xs = map unSimilar sims
                             in SimilarByPrivate k (unSimilar sim) xs
      )
  . Map.fromListWith (<>)
  . map (\s -> (similarKey s, [s]))
  . groupSimilar key 
