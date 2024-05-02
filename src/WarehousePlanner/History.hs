module WarehousePlanner.History 
( getBoxHistory
, computeBoxDiff
, computeBoxDiffHistoryFrom
, getShelfHistory
, computeShelfDiffHistoryFrom
, diffFor
) 
where 

import ClassyPrelude
import WarehousePlanner.Type
import Data.STRef
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map

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
   dsBoxUpdated = if box1 { _boxId = _boxId box2, boxShelf = boxShelf box2} == box2
                  then 0
                  else 1
   dsBoxDeleted = 0
   dsBoxCreated = 0
   (dsBoxOut, dsBoxIn) = if boxShelf box1 == boxShelf box2
                         then (mempty, mempty)
                         else (toSet $ boxShelf box2, toSet $ boxShelf box1)
   toSet = maybe mempty singletonSet
                         
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
  boxesFor :: forall s . Shelf s -> Set (BoxId s)
  boxesFor = setFromList . toList . _shelfBoxes
computeShelfDiffM :: Maybe (Shelf s) -> Maybe (Shelf s) -> DiffStatus (Set (ShelfId s))
computeShelfDiffM (Just shelf1) (Just shelf2) = computeShelfDiff shelf1 shelf2
computeShelfDiffM _ _ = mempty
 
  
-- | Drop all the event after pivot and compute the diff with the pivot for each event.
-- If the pivot is before everythyg, use them all but in reverse
computeDiffHistoryFor :: forall a s b . (Maybe (a s) -> Maybe (a s) -> DiffStatus b) -> Event -> History a s -> Map Event (DiffStatus b)
computeDiffHistoryFor compute pivot history = 
  let (future, past0) = span (\(_,e) -> e > pivot) $ NE.toList history 
      (current, past) = case past0 of
                             [] -> (Nothing , past0)
                             -- ((c,e):p) | e == pivot -> (Just c, p)
                             ((c,_):_) -> (Just c, past0)
                             -- ^^^ 
      mkPast past = [ (e, compute current p)
                    | (p, e) <- map (\(p,e) -> (Just p, e)) past
                                <> [(Nothing :: Maybe (a s), NoHistory)]
                    ]
      mkFuture future = [(e, compute (Just f) current) | (f, e) <- future  ]
  in mapFromList $ mkFuture future ++ mkPast past



computeShelfDiffHistoryFrom :: Event -> History Shelf s -> Map Event (DiffStatus (Set (ShelfId s)))
computeShelfDiffHistoryFrom = computeDiffHistoryFor computeShelfDiffM

computeBoxDiffHistoryFrom :: Event -> History Box s -> Map Event (DiffStatus (Set (ShelfId s)))
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
                              case Map.lookupGE hrToDiff  eventMap of
                                  Just es@(ev, _status) | ev > hrCurrent -> Just es
                                  _ -> Nothing
       | otherwise -> error "pattern should be exhaustive"
  
