module WarehousePlanner.History 
( getBoxHistory
, computeDiff
, computeDiffHistory
) 
where 

import ClassyPrelude
import WarehousePlanner.Type
import Data.STRef
import Data.List.NonEmpty (NonEmpty(..))


getBoxHistory :: Box' box => box s -> WH (History Box s) s
getBoxHistory box = do
   let BoxId (HiSTRef ref) = boxId box
   lift $ readSTRef ref
  
  
computeDiff :: Box s -> Box s -> DiffStatus (Set (ShelfId s))
computeDiff box1 box2 = DiffStatus{..} where
   dsBoxUpdated = if box1 { _boxId = _boxId box2, boxShelf = boxShelf box2} == box2
                  then 0
                  else 1
   dsBoxDeleted = 0
   (dsBoxOut, dsBoxIn) = if boxShelf box1 == boxShelf box2
                         then (mempty, mempty)
                         else (toSet $ boxShelf box2, toSet $ boxShelf box1)
   toSet = maybe mempty singletonSet
                         
-- | Computes all the diff vs the first (last) element, not consecutive ones
computeDiffHistory :: History Box s -> [(Event, DiffStatus (Set (ShelfId s)))]
computeDiffHistory (h@(box,_) :| history) =
   [ (event, computeDiff box past)
   | ((past,_), (_,event)) <- zip history (h : history)
   ]
  
  
  
