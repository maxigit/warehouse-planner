module WarehousePlanner.Brick.RenderBar
( shelfSummaryToBar
, renderHorizontalRun
, shelfSummaryToAllBars
, fromSummary
, charWithPerc2
, renderS
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
import Data.Text (takeEnd)


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
renderHorizontalRun run = hBox $ intersperse vBorder $ F.toList $ fmap (padTop Max . renderBay) (sShelves run)

renderBay :: Bay SumZip (SumZip ()) -> Widget n
renderBay bay = let
  ws =  reverse $ F.toList $ fmap renderShelf (sShelves bay)
  in vBox $ ws <> [txt $ takeEnd 3 $ dropEnd 1$ sName bay ]

renderShelf :: SumZip a -> Widget n
renderShelf = shelfSummaryToAllBars 


shelfSummaryToAllBars :: ShelvesSummary f a -> Widget n
shelfSummaryToAllBars sum =
  hBox [ go SVMaxLength SVSurfaceWH '>'  
       , go SVMaxWidth SVSurfaceWH 'o'
       , go SVMaxHeight SVSurfaceLW '^'
       ]
  where go m m' c = renderS m sum


charWithPerc2 :: Char -> Double -> Double -> Widget n
charWithPerc2 c r1 r2 = withAttr (percToAttrName r1 r2) (str [c])

renderS :: SummaryView -> ShelvesSummary f a -> Widget n
renderS smode s = let 
       c = case smode of
              SVMaxLength -> '>'
              SVMaxHeight -> '^'
              SVMaxWidth -> '○'
              SVSurfaceWH -> '◀'
              SVSurfaceLH -> '◆'
              SVSurfaceLW -> '▼'
              SVVolume -> '★'
       in charWithPerc2 c (ratio (fromSummary smode) s) 0
