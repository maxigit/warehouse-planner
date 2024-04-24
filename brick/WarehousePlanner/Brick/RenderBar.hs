{-# LANGUAGE MonadComprehensions, OverloadedLists #-}
module WarehousePlanner.Brick.RenderBar
( shelfSummaryToBar
, renderHorizontalRun
, shelfSummaryToAllBars
, fromSummary
, charWithPerc2
, renderS
, renderWithStyleName
, renderHorizontalSummary
, renderBestBar
)
where

import ClassyPrelude
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Summary
import Brick
import Brick.Widgets.Border as B
import WarehousePlanner.Type
import Data.Foldable qualified as F
import Data.Map qualified as Map
import Data.List.NonEmpty (NonEmpty(..))


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
renderHorizontalRun :: SummaryView -> Run SumVec (SumVec a) -> Widget n
renderHorizontalRun sview run = hBox $ intersperse vBorder $ F.toList $ fmap (padTop Max . renderBay sview ) (sDetails run)

renderBay :: SummaryView -> Bay SumVec (SumVec a) -> Widget n
renderBay sview bay = let
  ws =  reverse $ F.toList $ fmap (B.border . renderShelf) (sDetails bay)
  in joinBorders $ vBox $ ws <> [txt ( sName bay) <+> renderHorizontalSummary sview bay ]

renderShelf :: SumVec a -> Widget n
renderShelf ssum = vBox $ map ($ ssum) [renderWithStyleName , shelfSummaryToAllBars ]


shelfSummaryToAllBars :: ShelvesSummary f a -> Widget n
-- shelfSummaryToAllBars sum = hBox [ renderS v sum | v <- [minBound .. maxBound] ]
shelfSummaryToAllBars sum = hBox [ renderS SVVolume sum
                                 , renderBestBar [SVMaxLength, SVMaxWidth, SVMaxHeight ] sum
                                 , renderBestBar [SVSurfaceWH, SVSurfaceWH, SVSurfaceLW ] sum
                                 ]


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
       in charWithPerc2 c (ratio (fromSummary smode) s) 0 -- <+> hLimit 4 (str (show $ ratio (fromSummary smode) s))
       
renderHorizontalSummary :: SummaryView -> SumVec (SumVec a) -> Widget n
-- renderHorizontalSummary' sview = hBox . map (renderS sview) . sDetailsList 
renderHorizontalSummary _sview ssum = hBox . map (renderBestBar sviews) $ sDetailsList ssum  where
    sviews = SVSurfaceLW :| [ SVSurfaceLH, SVSurfaceWH ]
   
renderBestBar :: NonEmpty SummaryView -> ShelvesSummary f a -> Widget n
renderBestBar sviews ssum = let
   (_, sview) :| _ = sortOn fst
                   $ [ (ratio (fromSummary v)  ssum, v)
                      | v <- sviews
                      ]
  in renderS sview ssum

renderWithStyleName :: ShelvesSummary f a -> Widget n
renderWithStyleName s | null (sStyles s)  = str "∅"
renderWithStyleName s = hBox $ map forStyle $ Map.toList $ sStyles s where
  forStyle (style, bsum) = hBox [ styleNameWithAttr False style 
                                , str "x"
                                , str (show $ suCount bsum)
                                , str " " 
                                ]
