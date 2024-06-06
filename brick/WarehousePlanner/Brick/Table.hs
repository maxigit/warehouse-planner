module WarehousePlanner.Brick.Table
( shelfSummaryToTable
, renderBoxContent
, renderBoxOrientation
, baySummaryToTable
, runsToTable
, stylesToTable
)
where

import Brick
import Brick.Widgets.Table
import ClassyPrelude 
import Data.List (nub, transpose)
import Data.Map qualified as Map
import WarehousePlanner.Brick.RenderBar 
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Summary
import WarehousePlanner.Type
import WarehousePlanner.Base(boxShortContent)
import Data.Vector qualified as V
import Text.Printf (printf)


-- | Create a table with one cell per "offset"
-- each different value of an offset will create a new row/column
-- For exapmle (1,10) (3,8)) will create a 2x2 table 
-- with row correspending to 8 and 10 
-- and column to 1 and 3
-- One table is returned for each depth 
shelfSummaryToTable :: ([ZHistory1 Box RealWorld] ->  Widget n) -> SumVec (ZHistory1 Box RealWorld) -> [ Table n ]
shelfSummaryToTable renderBoxes ssum@ShelvesSummary{..} = let
   boxes = sDetailsList ssum
   boxesByOffset = Map.fromListWith (<>) [ (boxOffset $ zCurrentEx box , [box])
                                         | box <- boxes
                                         ]
   offsets = keys boxesByOffset
   xs = nub $ sort $ map dLength offsets
   ys = nub $ sort $ map dWidth offsets
   zs = nub $ sort $ map dHeight offsets
   cells = [ [ [ lookup (Dimension x y z) boxesByOffset
               | x <- xs
               ]
             | z <- reverse zs
             ]
           | y <- ys
           ]
   widgets = map (map (map $ renderBoxes . fromMaybe [] ))
           $ map (collapseColumns . collapseRows) cells
   in map (noBorders . table) widgets
                                         
noBorders = rowBorders False . columnBorders False
-- | merge rows if nothing collide
-- example
--       a _ b   => a x b
--       _ x _
--
collapseRows :: [[Maybe a]] -> [[Maybe a]]
collapseRows [] = []
collapseRows rows@[_] = rows
collapseRows (row1:row2:rows) =
  case mergeRows row1 row2 of
    Nothing -> row1 : collapseRows (row2:rows)
    Just merged -> collapseRows (merged:rows)

mergeRows xs ys = sequence $ zipWith mergeCells  xs ys where
   mergeCells xm ym = case (xm, ym) of 
                      (Just _, Just _) -> Nothing
                      _ -> Just (xm <|> ym)
    
collapseColumns = transpose . collapseRows . transpose


                    
renderBoxContent box = withStyleAttr (boxPropValue box) $ txt $ content <> " " where
     content = case getTagValuem box "ctitle" of
                   Nothing -> boxShortContent box
                   Just v -> v
renderBoxOrientation box = withStyleAttr (boxPropValue box)
                                 $ txt $ showOrientation' $ orientation box
  
  
-- | Displays on bay as a table
-- one row per shelf and one column per different depth
baySummaryToTable :: ([ZHistory1 Box RealWorld] -> Widget n) -> Bay SumVec (SumVec (ZHistory1 Box RealWorld)) -> Table n
baySummaryToTable renderBoxes ssum@ShelvesSummary{..} = let
  shelves = sDetailsList ssum
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


-- * Runs
-- | Displays a vertical table with all the runs
runsToTable :: HistoryRange -> ViewMode -> Int -> Runs SumVec _ -> Table Text
runsToTable hrange mode current runs = selectTable current mkRow  (sDetails runs)
    where mkRow i run = [ if mode == ViewHistory 
                          then historyIndicator (str "_") (isInSummary $ sName run) hrange (seEvents $ sExtra run)
                          else shelfSummaryToAllBars run 
                        , attr run i $ padLeftRight 1 $ txt (sName run)
                        , case mode of
                            ViewSummary smode -> renderHorizontalSummary smode run
                            ViewHistory -> hBox [ withShelfHLStatus bay $ historyIndicator (str "_") (isInSummary $ sName run) hrange (seEvents $ sExtra bay)
                                                | bay <- sDetailsList run
                                                ]
                        ]
          -- use selected style attribute if the run contains the style
          attr run i = let status = seBoxHLStatus $ sExtra run
                       in case status of
                            HighlightStatus{..} |  hsSelected + hsHighlighted > 0
                                                -> withHLStatus status  . ( <+> (str $ printf "(%d/%d)" hsSelected hsHighlighted   ))
                            _ -> selectAttr (current == i)


selectTable :: Int -> (Int -> a -> [Widget n ]) -> Vector a -> Table n
selectTable current f v = let
     rows = V.imap mkRow v
     mkRow i e = (str $ if i == current then "[" else " ")
                 : (f i e)
                 <> [ str if i == current then "]" else " "
                    ]
     in surroundingBorder False . noBorders . table $ toList  rows

stylesToTable :: Maybe Text -> Int -> Vector Text -> Table Text
stylesToTable _relected current styles = selectTable current mkRow styles
    where mkRow _ style = [ withStyleAttr style $ txt style ]
    
