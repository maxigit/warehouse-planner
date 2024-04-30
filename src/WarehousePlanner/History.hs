module WarehousePlanner.History 
( getBoxHistory
) 
where 

import ClassyPrelude
import WarehousePlanner.Type
import Data.STRef

getBoxHistory :: Box' box => box s -> WH (History Box s) s
getBoxHistory box = do
   let BoxId (HiSTRef ref) = boxId box
   lift $ readSTRef ref
  
  
