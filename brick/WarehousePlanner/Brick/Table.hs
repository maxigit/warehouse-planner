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
import WarehousePlanner.Summary as Summary
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
shelfSummaryToTable :: Bool -> DepthMode -> ([ZHistory1 Box RealWorld] ->  Widget n) -> SumVec (ZHistory1 Box RealWorld) -> [ Table n ]
shelfSummaryToTable collapseHeight depthMode renderBoxes ssum@ShelvesSummary{..} = let
   boxes = sDetailsList ssum
   boxesByOffset = Map.fromListWith (<>) [ (project . boxOffset $ zCurrentEx box , [box])
                                         | box <- boxes
                                         ]
   offsets = case keys boxesByOffset of
                   [] -> [Dimension 0 0 0]
                   ks -> ks
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
   widgets = map (map (map $ renderProjected . fromMaybe [] ))
           $ map (collapseColumns . collapseRows) $ (if collapseHeight then collapse else id ) cells
   in map (noBorders . table) widgets
   where renderProjected boxes = let
                         boxesByOffset = Map.fromListWith (<>) [ (boxOffset $ zCurrentEx box , [box])
                                                               | box <- boxes
                                                               ]
                         boxesByName = Map.fromListWith (<>) [ ([ fromMaybe (boxStyle box) $ getTagValuem box "ctitle"
                                                               , boxPropValue box
                                                               ]
                                                               , [zbox])
                                                             | zbox <- boxes
                                                             , let box = zCurrentEx zbox
                                                             ]
                         in case toList boxesByOffset of
                              [] -> withAttr (percToAttrName 0 0) $ str "·" -- emptyWidget
                              [ones]  -> renderBoxes ones
                              multi -> case depthMode of
                                            DMDistinctAndCount ->  let  renderWithNumber [one] = renderBoxes [one]
                                                                        renderWithNumber ms = (case length ms of
                                                                                                 1 -> emptyWidget
                                                                                                 l -> str (show l) <+> str "x"
                                                                                              ) <+> renderBoxes (take 1 ms)
                                                                   in str "[" <+> hBox (map renderWithNumber $ toList boxesByName) <+> str "]"
                                            DMDistinct ->  let  render = renderBoxes . take 1
                                                                   in str "[" <+> hBox (map render $ toList boxesByName) <+> str "]"
                                            DMFirstAndCount -> let prefix = case length boxes of
                                                                               1 -> emptyWidget
                                                                               l -> str (show (l-1)) <+> str "+"
                                                               in str "[" <+> prefix <+> renderBoxes (take 1 boxes)
                                            DMFirst -> renderBoxes (take 1 boxes)
                                            _ -> str "[" <+> hBox (map renderBoxes multi) <+> str "]"
         collapse xzy = let xyz = map transpose xzy
                        in map (transpose . map (sortOn (isJust)))  xyz
         project d = case depthMode  of
                       DMWide -> d
                       _ -> d {dWidth = 0 }


                                         
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
                   Nothing -> case boxShortContent box of
                                con | null con -> showOrientation' $ orientation box
                                con -> con
                   Just v -> v
renderBoxOrientation box = withStyleAttr (boxPropValue box)
                                 $ txt $ showOrientation' $ orientation box
  
  
-- | Displays on bay as a table
-- one row per shelf and one column per different depth
baySummaryToTable :: Bool -> DepthMode -> ([ZHistory1 Box RealWorld] -> Widget n) -> Bay SumVec (SumVec (ZHistory1 Box RealWorld)) -> Table n
baySummaryToTable collapseHeight depthMode renderBoxes ssum@ShelvesSummary{..} = let
  shelves = sDetailsList ssum
  tableCellsWithGap = map (\s -> map (\t -> (renderTable . rowBorders False $ surroundingBorder False $ t) <=> shelfBar s ) . shelfSummaryToTable collapseHeight depthMode renderBoxes $ s
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
  -- where shelfBar s = {- txt (Summary.sName s) <=> -} hBox [renderS m s | m <- [minBound .. maxBound] ] -- [ SVMaxLength .. SVMaxHeight] ]
  -- where shelfBar s = renderS SVVolume s <+> txt (Summary.sName s) <+>  hBox [renderS m s | m <- [ SVMaxLength .. SVMaxHeight] ] -- renderBestBarDef s
  where shelfBar s = txt (Summary.sName s) <+>  hBox [renderS m s | m <- [ SVMaxLength .. SVMaxHeight] ] -- renderBestBarDef s
  -- where shelfBar s = renderS SVVolume s <+> txt (Summary.sName s) <+>  renderBestBarDef s


-- * Runs
-- | Displays a vertical table with all the runs
runsToTable :: Maybe Text -> HistoryRange -> ViewMode -> Int -> Runs SumVec _ -> Table Text
runsToTable propm hrange mode current runs = selectTable current mkRow  (sDetails runs)
    where mkRow i run = [ if mode == ViewHistory 
                          then historyIndicator (str "_") (isInSummary $ sName run) hrange (seEvents $ sExtra run)
                          else shelfSummaryToAllBars run 
                        , attr run i $ padLeftRight 1 $ txt (sName run)
                        , case mode of
                            ViewSummary _ -> renderHorizontalSummary renderBestBarDef
                                                                         run
                            ViewHistory -> hBox [ withShelfHLStatus bay $ historyIndicator (str "_") (isInSummary $ sName run) hrange (seEvents $ sExtra bay)
                                                | bay <- sDetailsList run
                                                ]
                        ]
          -- use selected style attribute if the run contains the style
          attr run i = let status = seBoxHLStatus $ sExtra run
                       in case status of
                            HighlightStatus{..} |  hsSelected + hsHighlighted > 0
                                                -> let hl =  if hsHighlighted == 0
                                                             then str "0"
                                                             else maybe id withStyleAttr propm $ str (printf "%d" hsHighlighted)
                                                   in withHLStatus status  . ( <+> (str ( printf "(%d/" hsSelected) <+> hl <+> str ")"   ))
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
    
