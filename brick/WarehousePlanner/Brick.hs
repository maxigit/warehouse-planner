{-# LANGUAGE ExplicitForAll #-}
module WarehousePlanner.Brick
( renderWarehouse
, generateLevelAttrs
)
where


import ClassyPrelude hiding (on)
import WarehousePlanner.Base
import Brick
import Brick.Widgets.Border as Brick
import Brick.Util
import qualified Data.Foldable as Foldable
import Data.Text (commonPrefixes)

import qualified Graphics.Vty.Attributes as V
import qualified Graphics.Vty.Attributes.Color as V

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
renderBayTo2Halves tops bottoms = do
  -- calculate the percentage in 8th of bottom vs top shelfs
  let [nb, nt] = map length [bottoms, tops]
  let perc8 = case nb + nt of 
                  0 -> 0 
                  n -> 8 * nb `div` n
  usedBottom <- percUsed bottoms
  usedTop <- percUsed tops
  return $ withAttr (percToAttrName usedBottom usedTop) . str $ (eigthV perc8):[]

percUsed :: [Shelf s] -> WH Double s
percUsed shelves = do
  boxess <- mapM findBoxByShelf shelves
  let boxesV = sum $ map boxVolume $ concat boxess
      shelvesV = sum $ map shelfVolume shelves
  traceShowM (map shelfName (take 1 shelves), boxesV, shelvesV, boxesV / shelvesV * 100)
  if shelvesV < 1e-2 
  then return 0
  else return $ boxesV / shelvesV

  

eigthV :: Int -> Char
eigthV n | n <= 0  = ' '
eigthV 1 = '▁'
eigthV 2 = '▂'
eigthV 3 = '▃'
eigthV 4 = '▄'
eigthV 5 = '▅'
eigthV 6 = '▆'
eigthV 7 = '▇'
eigthV _ = '█'

eigthH :: Int -> Char
eigthH n | n <= 0  = ' '
eigthH 1 = '▏'
eigthH 2 = '▎'
eigthH 3 = '▍'
eigthH 4 = '▌'
eigthH 5 = '▋'
eigthH 6 = '▊'
eigthH 7 = '▉'
eigthH _ = '█'


data Level = Empty -- blue
           | Low  -- green
           | Medium -- yellow
           | Used -- orange
           | Full -- red
     deriving (Show, Eq, Read,Enum,Bounded)

percToLevel :: Double -> Level
percToLevel x | x <= 1e-2 = Empty
percToLevel x | x <= 0.30 = Low
percToLevel x | x <= 0.60 = Medium
percToLevel x | x <= 0.90 = Used
percToLevel _             = Full

levelsToAttrName l1 l2 = attrName (show l1) <> attrName (show l2)
percToAttrName p1 p2 = levelsToAttrName (percToLevel p1) (percToLevel p2)

generateLevelAttrs :: [(AttrName, V.Attr)]
generateLevelAttrs = 
    [ (levelsToAttrName l1 l2, fg `on` bg)
    | l1 <- [minBound..maxBound]
    , l2 <- [minBound..maxBound]
    , let fg = levelToColor l1
    , let bg= levelToColor l2
    ]
    
    
levelToColor = \case
  Empty -> V.brightBlue
  Low -> V.green
  Medium -> V.yellow
  Used -> V.magenta
  Full -> V.red




    
  
  
