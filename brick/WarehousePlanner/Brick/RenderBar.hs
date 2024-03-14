module WarehousePlanner.Brick.RenderBar
( shelfSummaryToBar
)
where

import ClassyPrelude
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Summary
import WarehousePlanner.Brick
import Brick
import Brick.Widgets.Border as Brick


shelfSummaryToBar :: BarDirection -> SummaryView -> ShelvesSummary f a -> Widget n
shelfSummaryToBar dir view ssum =  let
  r = ratio (fromSummary view) ssum
  perc8 = floor $ 8 * r
  bar = case dir of
         HorizontalBar -> eigthH
         VerticalBar -> eigthV
  in txt (sName ssum) <+> withAttr (percToAttrName r 0) (str (bar perc8 : []))
  
  
  
fromSummary :: SummaryView -> Summary -> Double
fromSummary mode = case mode of
  SVVolume -> suVolume 
  SVMaxLength -> suMaxLength
  SVMaxWidth -> suMaxWidth
  SVMaxHeight -> suMaxHeight
  SVSurfaceLW -> suSurfaceLW
  SVSurfaceLH -> suSurfaceLH
  SVSurfaceWH -> suSurfaceWH

  
  





