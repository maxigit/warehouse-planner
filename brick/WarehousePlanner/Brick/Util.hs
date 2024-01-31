module WarehousePlanner.Brick.Util
(
eigthH, eigthV
, percUsed
, percToLevel
, percToAttrName
, generateLevelAttrs
) where

import ClassyPrelude hiding (on)
import WarehousePlanner.Base
import Brick
import Brick.Widgets.Border as Brick
import Brick.Util
import qualified Graphics.Vty.Attributes as V
import qualified Graphics.Vty.Attributes.Color as V

percUsed :: [Shelf s] -> WH Double s
percUsed shelves = do
  boxess <- mapM findBoxByShelf shelves
  let boxesV = sum $ map boxVolume $ concat boxess
      shelvesV = sum $ map shelfVolume shelves
  -- traceShowM (map shelfName (take 1 shelves), boxesV, shelvesV, boxesV / shelvesV * 100)
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
    , let fg = levelToColor 255 l1
    , let bg= levelToColor 127 l2
    ]
    
    
levelToColor v = \case
  Empty -> V.color240 0 v v -- cyan
  Low -> V.color240 0 v 0 -- green
  Medium -> V.color240 v v 0  -- yellow
  Used -> V.color240 v (v `div` 2) 0 -- orange
  Full -> V.color240 v 0 0 -- red




    
  
  
