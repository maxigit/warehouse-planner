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
, renderBestBarDef
, bayToBars
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
shelfSummaryToBar :: BarDirection -> SummaryView -> SummaryView -> ShelvesSummary e f a -> Widget n
shelfSummaryToBar dir view cview ssum =  let
  r = ratio (fromSummary cview) ssum
  perc8 = floor $ 8 * (ratio (fromSummary view) ssum)
  bar = case dir of
         HorizontalBar -> eigthH
         VerticalBar -> eigthV
  in withDefAttr (percToAttrName r 0) (str (bar perc8 : []))
  
  
  
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
renderHorizontalRun :: Run SumVec (SumVec a) -> Widget n
renderHorizontalRun run = hBox $ intersperse vBorder $ F.toList $ fmap (padTop Max . renderBay ) (sDetails run)

renderBay :: Bay SumVec (SumVec a) -> Widget n
renderBay bay = let
  ws =  reverse $ F.toList $ fmap (B.border . renderShelf) (sDetails bay)
  in joinBorders $ vBox $ ws <> [withShelfHLStatus bay (txt ( sName bay)) <+> renderHorizontalSummary renderBestBarDef bay ]

renderShelf :: SumVec a -> Widget n
renderShelf ssum = vBox $ map ($ ssum) [renderWithStyleName , shelfSummaryToAllBars ]

-- | Display a shelf as a horizontal or vertical bar
-- to show the use.
-- shelfHOrVBar :: SumVec a -> Widget n

bayToBars :: SummaryView -> Bay SumVec (SumVec a) -> Widget n
bayToBars sview bay =  let
  -- ws = reverse $ F.toList $ fmap (renderBestBar [SVMaxHeight, SVMaxLength]) (sDetails bay)
  ws = reverse $ F.toList $ fmap render (sDetails bay)
  render ssum = padTop Max $ str (eigthV 1 : []) <=> (withShelfHLStatus ssum (
              if ratio (fromSummary SVMaxLength) ssum < ratio (fromSummary SVMaxHeight) ssum
              then shelfSummaryToBar HorizontalBar SVMaxLength sview ssum
              else shelfSummaryToBar VerticalBar SVMaxHeight sview ssum
              ) <+> str (eigthH 1 : []) )
  _render = renderBestBarDef
  in vBox ws

shelfSummaryToAllBars :: ShelvesSummary e f a -> Widget n
-- shelfSummaryToAllBars sum = hBox [ renderS v sum | v <- [minBound .. maxBound] ]
shelfSummaryToAllBars sum = hBox [ renderS SVVolume sum
                                 , renderBestBar [SVMaxLength, SVMaxWidth, SVMaxHeight ] sum
                                 , renderBestBar [SVSurfaceWH, SVSurfaceWH, SVSurfaceLW ] sum
                                 ]


charWithPerc2 :: Char -> Double -> Double -> Widget n
charWithPerc2 c r1 r2 = withDefAttr (percToAttrName r1 r2) (str [c])

renderS :: SummaryView -> ShelvesSummary e f a -> Widget n
renderS smode s = let 
       c = if  ratio (fromSummary smode) s <= 1e-6
           then '·'
           else case smode of
                     SVMaxLength -> '>'
                     SVMaxHeight -> '^'
                     SVMaxWidth -> 'o'
                     SVSurfaceWH -> '<'
                     SVSurfaceLH -> '#'
                     SVSurfaceLW -> 'v'
                     SVVolume -> '*'
       in charWithPerc2 c (ratio (fromSummary smode) s) 0 -- <+> hLimit 4 (str (show $ ratio (fromSummary smode) s))
       
renderHorizontalSummary ::  (SumVec a -> Widget n) -> SumVec (SumVec a) -> Widget n
renderHorizontalSummary renderBay ssum = hBox . map (\s -> withShelfHLStatus s $ renderBay s) $ sDetailsList ssum  where
    -- SVSurfaceLW :| [ SVSurfaceLH, SVSurfaceWH ]
   
-- |  Find the dimension with the least ratio
-- unless ones is over > 1. It needs to be displayed
renderBestBar :: NonEmpty SummaryView -> ShelvesSummary e f a -> Widget n
renderBestBar sviews ssum =
  let sview = case filter (\(r, _) -> r > 1) sviewsWithRatios of
                (_, v) : _ -> v
                _          -> snd . headEx $ sortedViews
      sviewsWithRatios = [(ratio (fromSummary v) ssum, v) | v <- toList sviews]
      sortedViews = sortOn fst sviewsWithRatios
  in renderS sview ssum

renderBestBarDef :: ShelvesSummary e f a -> Widget n
renderBestBarDef = renderBestBar (SVMaxLength :| [SVMaxWidth, SVMaxHeight])

renderWithStyleName :: ShelvesSummary SummaryExtra f a -> Widget n
renderWithStyleName s | null (sPropValues s)  = str "∅"
renderWithStyleName s = hBox $ map forStyle $ Map.toList $ sPropValues s where
  forStyle (style, bsum) = hBox [ styleNameWithAttr style 
                                , str "x"
                                , str (show $ suCount bsum)
                                , str " " 
                                ]
                                
                                
