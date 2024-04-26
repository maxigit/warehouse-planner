module WarehousePlanner.History 
(
getBoxHistory
) 
where 

import ClassyPrelude
import WarehousePlanner.Type
import Data.STRef
import Data.List.NonEmpty as NE

getBoxHistory :: Box' box => box s -> WH (NonEmpty (Box s, Event)) s
getBoxHistory box = do
   let BoxId (HiSTRef ref) = boxId box
   lift $ readSTRef ref
  
  
