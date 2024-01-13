module WarehousePlanner.Brick
(
renderWarehouse
)
where


import ClassyPrelude
import WarehousePlanner.Base
import Brick
import Brick.Widgets.Border as Brick


renderWarehouse :: WH (Widget n) s
renderWarehouse = do
   groups <- gets shelfGroup
   -- g <- gets shelves
   -- let groups = ShelfGroup (map ShelfProxy $ toList g) (error "X")
   slices <- mapM renderSlice $ toGroups groups
   return $ vBox $ slices
   
renderSlice :: ShelfGroup s -> WH (Widget n) s
renderSlice slice = do
  bays <- mapM renderBay $ toGroups slice
  return $ hBox $ {- intersperse vBorder -} bays
  
renderBay :: ShelfGroup s -> WH (Widget n) s
renderBay bay = do
  shelves <- mapM renderShelf $ toGroups bay
  return $ Brick.border $  vBox $ {- intersperse hBorder -} shelves
  
renderShelf :: ShelfGroup s -> WH (Widget n) s
renderShelf (ShelfProxy sid) = do
    shelf <- findShelf sid 
    return $ txt $ shelfName shelf
renderShelf group = renderSlice group
   
  








    
  
  
