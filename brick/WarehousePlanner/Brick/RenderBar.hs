module WarehousePlanner.Brick.RenderBar
( shelfSummaryToBar
, renderHorizontalRun
)
where

import ClassyPrelude
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Summary
import WarehousePlanner.Brick
import Brick
import Brick.Widgets.Border as B
import WarehousePlanner.Type
import qualified Data.Foldable as F


-- * Bar
shelfSummaryToBar :: BarDirection -> SummaryView -> ShelvesSummary f a -> Widget n
shelfSummaryToBar dir view ssum =  let
  r = ratio (fromSummary view) ssum
  perc8 = floor $ 8 * r
  bar = case dir of
         HorizontalBar -> eigthH
         VerticalBar -> eigthV
  in withAttr (percToAttrName r 0) (str (bar perc8 : []))
  
  
  
fromSummary :: SummaryView -> Summary -> Double
fromSummary mode = case mode of
  SVVolume -> suVolume 
  SVMaxLength -> suMaxLength
  SVMaxWidth -> suMaxWidth
  SVMaxHeight -> suMaxHeight
  SVSurfaceLW -> suSurfaceLW
  SVSurfaceLH -> suSurfaceLH
  SVSurfaceWH -> suSurfaceWH

  
  
-- * 
renderHorizontalRun :: Run SumZip (SumZip ()) -> Widget n
renderHorizontalRun run = hBox $ F.toList $ fmap (B.border . padBottom Max . renderBay) (sShelves run)

renderBay :: Bay SumZip (SumZip ()) -> Widget n
renderBay bay = let
  ws =  F.toList $ fmap renderShelf (sShelves bay)
  in vBox $ ws <> [txt $ sName bay ]

renderShelf :: SumZip a -> Widget n
renderShelf = shelfSummaryToBar VerticalBar SVMaxLength 



