module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, SummaryView(..)
, BarDirection(..)
, SumVec, sDetailsList
, currentRun, currentBay, currentShelf
, selectedStyle, currentStyle
)
where

import ClassyPrelude
import WarehousePlanner.Summary
import WarehousePlanner.Type
import Data.Vector qualified as V
import Data.Foldable qualified as F

data BarDirection = HorizontalBar
             | VerticalBar
     deriving (Show, Eq, Enum, Bounded)
     
data SummaryView = SVVolume
                 | SVMaxLength
                 | SVMaxWidth
                 | SVMaxHeight
                 | SVSurfaceLW
                 | SVSurfaceLH
                 | SVSurfaceWH
                 -- | SVLW
     deriving (Show, Eq, Enum, Bounded)
     
data ViewMode = ViewSummary SummaryView
              -- | ViewSplitShelves (Shelf s -> Bool)
              -- | ViewSplitBoxes (Box s -> Bool)
              -- 
-- | Summary with a "list" with current position
type SumVec = ShelvesSummary Vector
data AppState = AppState
     { -- asViewMode  :: ViewMode
     asSummaryView :: SummaryView
     , asShelvesSummary :: Runs SumVec (SumVec (Box RealWorld)) 
     , asCurrentRun :: Int
     , asCurrentBay :: Int
     , asCurrentShelf :: Int
     , asSelectedStyle :: Maybe Text
     , asCurrentStyle :: Int 
     , asCurrentRunStyles :: Vector Text
     }
     
selectFromSumVec :: Int -> SumVec a -> a
selectFromSumVec i ShelvesSummary{sDetails} = sDetails V.! (i `min` (length sDetails - 1))

currentRun :: AppState -> Run SumVec (Bay SumVec _)
currentRun state = selectFromSumVec (asCurrentRun state) (asShelvesSummary state)

-- currentBay :: AppState -> Bay SumVec _a
currentBay state = selectFromSumVec (asCurrentBay state) (currentRun state)

currentShelf :: AppState -> _a
currentShelf state = selectFromSumVec (asCurrentShelf state) (currentBay state)

sDetailsList :: Foldable f => ShelvesSummary f a -> [a]
sDetailsList ssum = F.toList (sDetails ssum)

selectedStyle :: AppState -> Maybe Text
selectedStyle state@AppState{..} = asSelectedStyle <|> currentStyle state

currentStyle :: AppState -> Maybe Text
currentStyle AppState{..} = asCurrentRunStyles V.!? (asCurrentStyle `mod` length asCurrentRunStyles)


