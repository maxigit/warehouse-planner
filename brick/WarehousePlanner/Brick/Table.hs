module WarehousePlanner.Brick.Table
( shelfSummaryToTable
, renderBoxContent
, renderBoxOrientation
, baySummaryToTable
)
where

import Brick
import Brick.Widgets.Table
import ClassyPrelude 
import Data.Map qualified as Map
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Summary
import WarehousePlanner.Type
import Data.List (nub)
import Data.Foldable qualified as F


-- | Create a table with one cell per "offset"
-- each different value of an offset will create a new row/column
-- For exapmle (1,10) (3,8)) will create a 2x2 table 
-- with row correspending to 8 and 10 
-- and column to 1 and 3
-- One table is returned for each depth 
shelfSummaryToTable :: ([Box RealWorld] ->  Widget n) -> SumZip (Box RealWorld) -> [ Table n ]
shelfSummaryToTable renderBoxes ShelvesSummary{..} = let
   boxes = F.toList $ sShelves 
   boxesByOffset = Map.fromListWith (<>) [ (boxOffset box , [box])
                                         | box <- boxes
                                         ]
   offsets = keys boxesByOffset
   xs = nub $ sort $ map dLength offsets
   ys = nub $ sort $ map dWidth offsets
   zs = nub $ sort $ map dHeight offsets
   widgets = [ [ [ renderBoxes $ findWithDefault [] (Dimension x y z) boxesByOffset
                 | x <- xs
                 ]
               | z <- reverse zs
               ]
             | y <- ys
             ]
   in map (rowBorders False . columnBorders False . table) widgets
                                         

                    
renderBoxContent box = withStyleAttr (boxStyle box) $ txt $ boxContent box <> " "
renderBoxOrientation box = withStyleAttr (boxStyle box) $ txt $ showOrientation' $ orientation box
  
  
-- | Displays on bay as a table
-- one row per shelf and one column per different depth
baySummaryToTable :: ([Box RealWorld] -> Widget n) -> Bay SumZip (SumZip (Box RealWorld)) -> Table n
baySummaryToTable renderBoxes ShelvesSummary{..} = let
  shelves = F.toList $ sShelves
  tableCellsWithGap = map (map (renderTable . surroundingBorder False)
                    . shelfSummaryToTable renderBoxes
                    ) $ reverse shelves
  -- in case the depths is not the same for all shelves
  -- we need fill create empty cell if necessary
  -- so that `table` doesn't complain
  maxDepth = case tableCellsWithGap of
                  [] -> 0
                  _ -> maximumEx $ map length tableCellsWithGap
  fillGaps row = take maxDepth $ row <> repeat emptyWidget
  tableCells = map fillGaps tableCellsWithGap
                    
  in case maxDepth of 
          0 -> table [[emptyWidget]]
          _ -> table tableCells

