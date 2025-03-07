{-# LANGUAGE DeriveTraversable #-}
module WarehousePlanner.Slices
( Slices(..)
, breakSlices
, partitionEitherSlices
, filterSlices
, filterSlicesWithKey
, dropTillSlice
, dropTillSlot
, unconsSlicesTo
, unconsSlice
, unconsSlices
, buildSlices
, numSlices
, groupSlicesWithKey
)where 
import ClassyPrelude hiding (uncons, stripPrefix )
import WarehousePlanner.Type (BoxBreak(..))
import Data.Foldable qualified as F
-- | An ordered list. Modifying it using fmap doesn't reorder it.
-- It is so that we can work with infinite list.
-- Therefore fmap should be used with caution and make sure
-- the order is kept.
newtype OrderedList a = OrderedList [a] deriving (Eq, Show, Foldable, Functor, Traversable)

newtype Slot k a = Slot (OrderedList (k, a))
  deriving (Eq, Show, Functor, Foldable, Traversable)
newtype Slice k a = Slice (OrderedList (k, Slot k a))
  deriving (Eq, Show, Functor, Foldable, Traversable)
-- | Everything within slices are supposed to be sorted according
-- the k. When using fmap (etc) only monotone function should be used.
newtype Slices k a = Slices (OrderedList (k, Slice k a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

{-# COMPLETE SlotO #-}
pattern SlotO xs = Slot (OrderedList xs)
{-# COMPLETE SliceO #-}
pattern SliceO xs  = Slice (OrderedList xs)
{-# COMPLETE SlicesO #-}
pattern SlicesO xs = Slices (OrderedList xs)
  
mergeWith :: Ord k => (a -> a -> [a]) -> OrderedList (k, a) -> OrderedList (k, a) -> OrderedList (k, a)
mergeWith combine (OrderedList xs) (OrderedList ys) = OrderedList (go xs ys) where
    go [] ys = ys
    go xs [] = xs
    go (x:xs) (y:ys) = 
      case compare (fst x) (fst y) of
        LT -> x : go xs (y:ys)
        EQ -> (map (\v -> (fst x,v)) ( snd x `combine` snd y)) <> go xs ys
        GT -> y : go (x:xs) ys


instance Ord k => Semigroup (Slot k a) where
  Slot xs <> Slot ys = Slot (mergeWith (\a b -> [a,b]) xs ys)
  
instance Ord k => Monoid (Slot k a) where
  mempty = Slot (OrderedList [])
  
instance Ord k => Semigroup (Slice k a) where
  Slice xs <> Slice ys = Slice (mergeWith (\a b -> [a <> b]) xs ys)
  
instance Ord k => Monoid (Slice k a) where
  mempty = Slice (OrderedList [])
  
instance Ord k => Semigroup (Slices k a) where
  Slices xs <> Slices ys = Slices (mergeWith (\a b -> [a <> b]) xs ys)
  
instance Ord k => Monoid (Slices k a) where
  mempty = Slices (OrderedList [])
  
instance Bifunctor Slot where
  bimap l r (Slot key'poss) = Slot $ fmap (bimap l r) key'poss
  
instance Bifunctor Slice where
  bimap l r (Slice key'slots) = Slice $ fmap (bimap l (bimap l r)) key'slots
  
instance Bifunctor Slices where
  bimap l r (Slices key'slices) = Slices $ fmap (bimap l (bimap l r)) key'slices
  
-------------------------------------------------- {{{ 1
-- * Utilities         
--------------------------------------------------
buildSlices :: Int -> Int -> Int -> (Int -> k) -> (Int -> Int -> k) ->  (Int -> Int -> Int -> (k, a)) -> Slices k a
buildSlices nbOfSlices nbOfSlots nbPerSlot mkSliceIndex mkSlotIndex mkPos = let
  mkSlice sliceIndex = 
    (mkSliceIndex sliceIndex, Slice . OrderedList $ map (mkSlot sliceIndex) [0..nbOfSlots - 1])
  mkSlot sliceIndex slotIndex =
    (mkSlotIndex sliceIndex slotIndex, Slot . OrderedList $ map (mkPos sliceIndex slotIndex ) [0..nbPerSlot - 1])
  in Slices . OrderedList $ map mkSlice [0..nbOfSlices - 1]

numSlices :: (Num n, Enum n) => Slices k a -> Slices (n, k) a
numSlices (SlicesO slices) = SlicesO $ [bimap (i,) numSlice slice | (slice, i) <- zip slices [0..]]

numSlice :: (Num n, Enum n) => Slice k a -> Slice (n, k) a
numSlice (SliceO slots) = SliceO $ [bimap (i,) numSlot slot | (slot, i) <- zip slots [0..]]

numSlot :: (Num n, Enum n) => Slot k a -> Slot (n, k) a
numSlot (SlotO poss) = SlotO $ [bimap (i,) id pos | (pos, i) <- zip poss [0..]]
unconsSlot :: Slot k a -> Maybe (a,k, Slot k a)
unconsSlot (Slot (OrderedList key'poss)) =
  case key'poss of
    [] -> Nothing
    ((key,pos):kps) -> Just (pos,key, Slot (OrderedList kps))
    
unconsSlice :: Slice k a -> Maybe (a,(k,k), Slice k a)
unconsSlice (Slice (OrderedList key'slots)) =
  case key'slots of
    [] -> Nothing
    ((key, slot0):kss) -> 
      case unconsSlot slot0 of
        Nothing -> unconsSlice (Slice $ OrderedList kss)
        Just (pos, k1, slot) -> Just (pos, (key, k1), Slice (OrderedList $ (key, slot):kss))
    
unconsSlices :: Slices k a -> Maybe (a, (k,k,k), Slices k a)
unconsSlices (Slices (OrderedList key'slices)) =
  case key'slices of
    [] -> Nothing
    ((key, slice0):kss) ->
      case unconsSlice slice0 of
        Nothing -> unconsSlices (Slices $ OrderedList kss)
        Just (pos, (k1,k2), slice) -> Just (pos, (key,k1,k2), Slices (OrderedList $ (key, slice):kss))


  

unconsSlicesTo :: Eq shelf => (a -> shelf) -> Maybe shelf -> Maybe BoxBreak -> Slices (Int, k) a -> Maybe (a, ((Int, k),(Int, k),(Int, k)), Slices (Int, k) a)
unconsSlicesTo _ _ Nothing slices = unconsSlices slices
unconsSlicesTo _ _ _ (Slices (OrderedList [])) = Nothing
unconsSlicesTo getShelf  Nothing (Just StartNewShelf) slices = unconsSlicesTo getShelf Nothing (Just StartNewSlice) slices
unconsSlicesTo getShelf js@(Just prevShelf) (Just StartNewShelf) slices = 
  -- try next slice until different shelf
  case unconsSlicesTo getShelf Nothing (Just StartNewSlice) slices of
    Nothing -> Nothing
    Just new@(a, _, _) | getShelf a /= prevShelf -> Just new
    Just (_, _, newSlices) -> 
      unconsSlicesTo getShelf js (Just StartNewShelf) newSlices
unconsSlicesTo _ _ (Just StartNewSlice) slices =
  case unconsSlices slices of
    Nothing -> Nothing
    Just new@(_, (_,(0,_),_), _) -> Just new
    --               ^
    --               +-- new slices mean slot number = 0
    _ -> unconsSlices (dropTillSlice slices)
unconsSlicesTo _ _ (Just StartNewSlot) slices =
  case unconsSlices slices of
    Nothing -> Nothing
    Just new@(_, (_,_,(0,_)), _) -> Just new
    _ -> unconsSlices (dropTillSlot slices)

dropTillSlice, dropTillSlot :: Slices k a -> Slices k a
dropTillSlice (SlicesO slices) = cleanSlices $ SlicesO (drop 1 slices)
dropTillSlot (SlicesO ((i, SliceO (_:slots)):slices)) = cleanSlices $ SlicesO (slice:slices) where
  slice = (i, SliceO slots)
dropTillSlot _ = SlicesO []

-- | Remove empty sublists
cleanSlices :: Slices k a -> Slices k a
cleanSlices (SlicesO ((_, SliceO []):slices)) = cleanSlices $ SlicesO (map (second cleanSlice) slices)
cleanSlices slices = slices

cleanSlice :: Slice k a -> Slice k a
cleanSlice (SliceO ((_, SlotO []):slots)) = cleanSlice $ SliceO slots
cleanSlice slice = slice

  

filterSlices :: (a -> Bool)  -> Slices k a -> Slices k a
filterSlices keep = filterSlicesWithKey (\_ a ->  keep a)
filterSlicesWithKey :: (k -> a -> Bool)  -> Slices k a -> Slices k a
filterSlicesWithKey keep (SlicesO xs) = cleanSlices $ SlicesO $ map (second $ uncleanFilterSliceWithKey keep) xs


partitionEitherSlices :: Slices k (Either a b) -> (Slices k a, Slices k b)
partitionEitherSlices (SlicesO xs) = let
 (as, bs) = unzip $ [ ((k, lefts), (k, rights))
                    | (k, slice) <- xs 
                    , let (lefts, rights) = uncleanPartitionESlice slice
                    ] 
 in (cleanSlices $ SlicesO as, cleanSlices $ SlicesO bs)

breakSlices :: (a -> Bool) -> Slices k a -> (Slices k a, Slices k a)
breakSlices p s@(SlicesO xs) = case xs of
   [] -> (s,s)
   -- breaking a slice can give 3 results
   -- ex > 3 [1..10]
   --    a) ([1,2,3], [4..10]) break found in the middle
   -- ex > 3 [1..2]
   --    b) ([1,2], []) no break found
   -- ex > 3 [5..10]
   --    c) ([], [5..10]) break found straight away
   (k, slice):slices -> case breakSlice p slice of
                (_ , SliceO []) -> -- c) no break found, try harder
                    let (SlicesO befores, after) = breakSlices p $ SlicesO slices
                    in ( SlicesO $ (k, slice):befores
                       , after
                       ) 
                (before, after) -> -- a,b ) we found the break no need break the neslicet one
                     ( cleanSlices $ SlicesO [(k, before)]
                     , SlicesO $ (k, after):slices
                     )
breakSlice :: (a -> Bool ) -> Slice  k a -> (Slice k a, Slice k a)
breakSlice p s@(SliceO xs) = case xs of
   [] -> (s,s)
   (k,slot):slots -> case breakSlot p slot of
                (_, SlotO []) ->  -- c) no break found, try harder
                    let (SliceO befores, after) = breakSlice p$ SliceO  slots
                    in ( SliceO $ (k,slot):befores
                       , after
                       ) 
                (before, after) -> -- we found the break no need break the next one
                     ( SliceO [(k,before)]
                     , SliceO $ (k, after):slots
                     )
                     
breakSlot :: (a -> Bool) -> Slot k a -> (Slot k a, Slot k a)
breakSlot p (SlotO xs) = let
 (before, after) = break (p .snd)  xs
 in (SlotO before, SlotO after)

 
-- | Break slices when function changes. Doesn't sort
groupSlicesWithKey :: Eq r => (a -> r) -> Slices k a -> [(r, Slices k a)]
groupSlicesWithKey f slices = case F.toList slices of
     [] -> []
     x:_ -> let r = f x
                (before, after) = breakSlices (\a -> f a /= r) slices
            in (r, before) : groupSlicesWithKey f after

--  * Unclean functions, need to be cleaned afterward
uncleanFilterSliceWithKey :: (k -> a -> Bool) -> Slice k a -> Slice k a
uncleanFilterSliceWithKey keep (SliceO xs) = SliceO (map (second $ uncleanFilterSlotWithKey keep) xs)

uncleanFilterSlotWithKey :: (k  -> a -> Bool) -> Slot k a -> Slot k a
uncleanFilterSlotWithKey keep (SlotO xs)  = SlotO $ filter (uncurry keep) xs


uncleanPartitionESlot :: forall a k b . Slot k (Either a b) -> (Slot k a, Slot k b)
uncleanPartitionESlot (SlotO xs) = let
   as :: [(k, a)]
   bs :: [(k, b)]
   (as, bs) = partitionEithers $ map biseq xs
   in (SlotO as, SlotO bs)
   
uncleanPartitionESlice :: Slice k (Either a b) -> (Slice k a, Slice k b)
uncleanPartitionESlice (SliceO xs) = let
 (as, bs) = unzip $ [ ((k, lefts), (k, rights))
                    | (k, slot) <- xs 
                    , let (lefts, rights) = uncleanPartitionESlot slot
                    ] 
 in (SliceO as, SliceO bs)

biseq :: (x, Either a b) -> Either (x, a) (x, b)
biseq (x, e)= bimap (x,) (x,) e
