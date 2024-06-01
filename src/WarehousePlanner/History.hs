module WarehousePlanner.History 
( getBoxHistory
, computeBoxDiff, computeBoxDiffM
, computeBoxDiffHistoryFrom
, getShelfHistory
, computeShelfDiffHistoryFrom
, diffFor
, toZHistory
, historyFor
, findNextEvent, findNextSibling, findPreviousSibling, findFirstChild
, mergeEventMaps
) 
where 

import ClassyPrelude
import WarehousePlanner.Type
import Data.STRef
import Data.Map qualified as Map
import Control.Monad.State(gets)
import Data.List.NonEmpty qualified as NE

_reverseDiff :: DiffStatus a -> DiffStatus a
_reverseDiff d = d { dsBoxOut = dsBoxIn d, dsBoxIn = dsBoxOut d }

--------------------------------------------------
getBoxHistory :: Box' box => box s -> WH (History Box s) s
getBoxHistory box = do
   let BoxId (HiSTRef ref) = boxId box
   lift $ readSTRef ref
  
getShelfHistory :: Shelf' shelf => shelf s -> WH (History Shelf s) s
getShelfHistory shelf = do
   let ShelfId (HiSTRef ref) = shelfId shelf
   lift $ readSTRef ref
-------------------------------------------------- 
computeBoxDiff :: Box s -> Box s -> DiffStatus (Set (ShelfId s))
computeBoxDiff box1 box2 = DiffStatus{..} where
   dsBoxUpdated = if cleanBox box1  == cleanBox box2
                  then 0
                  else 1
   (dsBoxCreated, dsBoxDeleted) = case (boxShelf box1, boxShelf box2) of
                                      (Nothing, Nothing) -> (0,0)
                                      (Nothing, _) -> (1,0)
                                      (_, Nothing) -> (0,1)
                                      _ -> (0,0)
    
   (dsBoxOut, dsBoxIn, dsBoxShuffled) = if boxShelf box1 == boxShelf box2
                         then ( mempty
                              , mempty
                              , if (orientation box1 /= orientation box2)
                                || (boxOffset box1 /= boxOffset box2)
                                then 1
                                else 0
                              )
                                           
                         else (toSet $ boxShelf box2, toSet $ boxShelf box1, 0)
   toSet = maybe mempty singletonSet
   cleanBox box = box { boxTags = Map.delete "@prop" (boxTags box) 
                      , boxOffset = boxOffset box2
                      , orientation = orientation box2
                      , boxShelf = boxShelf box2
                      , _boxId = _boxId box2
                      }
   --                                    ^^^^^ set by brick, so should be seen as a diff
                         
computeBoxDiffM :: Maybe (Box s) -> (Maybe (Box s) -> DiffStatus (Set (ShelfId s)))
computeBoxDiffM Nothing Nothing = mempty
computeBoxDiffM (Just box) Nothing = mempty { dsBoxIn = setFromList $ toList $ boxShelf box, dsBoxCreated = 1 }
computeBoxDiffM Nothing (Just box) = mempty { dsBoxIn = setFromList $ toList $ boxShelf box, dsBoxDeleted = 1 }
computeBoxDiffM (Just box1) (Just box2) = computeBoxDiff box1 box2

  
  
-- | Only check if boxes have moved out
computeShelfDiff :: Shelf s -> Shelf s -> DiffStatus (Set (ShelfId s))
computeShelfDiff shelf1 shelf2 = DiffStatus{..} where
  dsBoxOut  = case boxesFor shelf2 \\ boxesFor shelf1  of
                           --                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                           --                   box present initially - box left now
                   set | null set -> mempty
                   _ -> singletonSet $ shelfId shelf2
  dsBoxIn = case boxesFor shelf1 \\ boxesFor shelf2  of
                 set | null set -> mempty
                 _ -> singletonSet $ shelfId shelf1
  dsBoxUpdated = 0
  dsBoxDeleted = 0
  dsBoxCreated = 0
  dsBoxShuffled = 0
  boxesFor :: forall s . Shelf s -> Set (BoxId s)
  boxesFor = setFromList . toList . _shelfBoxes
computeShelfDiffM :: Maybe (Shelf s) -> Maybe (Shelf s) -> DiffStatus (Set (ShelfId s))
computeShelfDiffM (Just shelf1) (Just shelf2) = computeShelfDiff shelf1 shelf2
computeShelfDiffM _ _ = mempty
 
  
-- | Drop all the event after pivot and compute the diff with the pivot for each event.
-- If the pivot is before everythyg, use them all but in reverse
computeDiffHistoryFor :: forall a s b . (Maybe (a s) -> Maybe (a s) -> DiffStatus b) -> ZHistory1 a s -> Map Event (DiffStatus b)
computeDiffHistoryFor compute ZHistory{..} = 
  let current = fmap snd $ Map.lookupMax zBefore
      mkPast past = [ (e, compute current p)
                    | (e, p) <- map (\(e, p) -> (e, Just p)) (Map.toList past)
                                <> [(NoHistory, Nothing :: Maybe (a s))]
                    ]
      mkFuture future = [(e, compute (Just f) current) | (e, f) <- (Map.toList future)  ]
  in mapFromList $ mkFuture zAfter ++ mkPast zBefore



computeShelfDiffHistoryFrom :: ZHistory1 Shelf s -> Map Event (DiffStatus (Set (ShelfId s)))
computeShelfDiffHistoryFrom = computeDiffHistoryFor computeShelfDiffM

computeBoxDiffHistoryFrom :: ZHistory1 Box s -> Map Event (DiffStatus (Set (ShelfId s)))
computeBoxDiffHistoryFrom = computeDiffHistoryFor computeBoxDiffM


diffFor :: Monoid s => HistoryRange -> Map Event (DiffStatus s) -> (Event, DiffStatus s)
diffFor HistoryRange{..} eventMap = fromMaybe (NoHistory, mempty) $
     if
       | hrCurrent == hrToDiff -> Nothing
       | hrCurrent > hrToDiff -> -- look in the past
                              case Map.lookupLE hrToDiff eventMap of
                                   Just es@(ev, _status) | ev < hrCurrent ->  Just es
                                   _ -> Nothing
       | hrCurrent < hrToDiff -> -- look in the future
                              case Map.lookupLE hrToDiff  eventMap of
                                  Just es@(ev, _status) | ev > hrCurrent -> Just es
                                  _ -> Nothing
       | otherwise -> error "pattern should be exhaustive"
  
  
-- * Zippers
toZHistory :: Event -> History a s -> ZHistory (a s)
toZHistory ev history = let
  m = mapFromList (toList history)
  (before, currentm, zAfter) = Map.splitLookup ev m
  zBefore = maybe mempty (Map.singleton ev) currentm <> before
  in ZHistory{..}

historyFor :: History a s -> WH (History a s) s
historyFor history = do
   ev <- gets whCurrentEvent
   case NE.dropWhile ((> ev) . fst) history of
        [] -> return history
        (h:hs) -> return $ h NE.:| hs

-- * Find events
findNextEvent :: Event -> [Event] -> Maybe Event
findNextEvent ev = find \e -> evPreviousM e == Just ev

findFirstChild :: Event -> [Event] -> Maybe Event
findFirstChild ev = find \e -> evParent e == Just ev 

findNextSibling :: Event -> [Event] -> Maybe Event
findNextSibling ev = lastMay . fst . findSiblings ev

findPreviousSibling :: Event -> [Event] -> Maybe Event
findPreviousSibling ev = headMay . snd . findSiblings ev


findSiblings :: Event -> [Event] -> ([Event], [Event])
findSiblings ev events = let 
   siblings = filter (\e -> evParent e == evParent ev) events
   (after, before) = break (==ev) siblings
   in (after, drop 1 before)
   
   
-- * Event Map
-- | merge maps. The trick here is that missing events in map need
-- to be added  so that
-- {1 : a , 3 : b} + {2: c } = {1: a, 2 : a+c, 3: b}
--                                       ^^^^^
mergeEventMaps :: Ord k => Semigroup a =>  k -> [Map k a ] -> Map k a
mergeEventMaps pivot maps = let
    events = foldMap Map.keysSet maps
    in Map.fromListWith (<>) [ (ev, a)
                            | ev <- toList events
                            , evMap <- maps
                            , (found,a) <- toList $ Map.lookupLE ev evMap
                            , (ev > pivot && found > pivot) || (ev <= pivot && found <= pivot)
                            ]
