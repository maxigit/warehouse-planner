{-# LANGUAGE ExplicitForAll #-}
module WarehousePlanner.Brick
( renderWarehouse
, generateLevelAttrs
)
where


import ClassyPrelude hiding (on)
import WarehousePlanner.Base
import WarehousePlanner.Brick.Util
import Brick
import Brick.Widgets.Border as Brick
import qualified Data.Foldable as Foldable
import Data.Text (commonPrefixes)


renderWarehouse :: WH (Widget n) s
renderWarehouse = do
   groups <- gets shelfGroup
   -- g <- gets shelves
   -- let groups = ShelfGroup (map ShelfProxy $ toList g) (error "X")
   slices <- mapM renderSlice $ toGroups groups
   return $ vBox {- $ intersperse hBorder -} $  slices
   
renderSlice :: ShelfGroup s -> WH (Widget n) s
renderSlice slice = do
  bays <- mapM renderBay $ toGroups slice
  shelves <- mapM (findShelf @(ShelfId)) (Foldable.toList slice)
  let title = case map shelfName shelves of
                 (x:xs) -> foldl' commonPrefix x xs
                 [] -> ""
  return $ hBox $ {- intersperse vBorder -} txt (title <> ": ") : bays
  where commonPrefix a b = maybe "" (\(p,_,_) -> p) (commonPrefixes a b)
  
renderBay :: forall s n. ShelfGroup s -> WH (Widget n) s
renderBay bay = do
  shelves <- mapM (findShelf @(ShelfId)) (Foldable.toList bay)
  let (bottoms, tops) = partition (not . isTop) shelves
  renderBayTo2Halves bottoms tops
  where isTop s = tagIsPresent s "top"
  
renderShelf :: ShelfGroup s -> WH (Widget n) s
renderShelf (ShelfProxy sid) = do
    shelf <- findShelf sid 
    return $ txt $ shelfName shelf
renderShelf group = renderSlice group
   
  


-- | render one bay to 
renderBayTo2Halves :: [Shelf s ] -> [Shelf s] -> WH (Widget  n) s
renderBayTo2Halves bottoms tops = do
  -- calculate the percentage in 8th of bottom vs top shelfs
  let [nb, nt] = map length [bottoms, tops]
  let perc8 = case nb + nt of 
                  0 -> 0 
                  n -> 1 + (6 * nb `div` n)
  usedBottom <- percUsed bottoms
  usedTop <- percUsed tops
  return $ withAttr (percToAttrName usedBottom usedTop) . str $ (eigthV perc8):[]
