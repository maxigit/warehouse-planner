module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, SummaryView(..)
, BarDirection(..)
, SumVec, sShelfList
, currentRun, currentBay, currentShelf
)
where

import ClassyPrelude
import WarehousePlanner.Summary
import WarehousePlanner.Type
import Data.Vector qualified as V

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
     { asViewMode  :: ViewMode
     , asShelvesSummary :: Runs SumVec (SumVec (Box RealWorld)) 
     , asCurrentRun :: Int
     , asCurrentBay :: Int
     , asCurrentShelf :: Int
     }
     
selectFromSumVec :: Int -> SumVec a -> a
selectFromSumVec i ShelvesSummary{sShelves} = sShelves V.! (i `min` (length sShelves - 1))

currentRun :: AppState -> Run SumVec (Bay SumVec _)
currentRun state = selectFromSumVec (asCurrentRun state) (asShelvesSummary state)

-- currentBay :: AppState -> Bay SumVec _a
currentBay state = selectFromSumVec (asCurrentBay state) (currentRun state)

currentShelf :: AppState -> _a
currentShelf state = selectFromSumVec (asCurrentShelf state) (currentBay state)

sShelfList :: SumVec a -> [a]
sShelfList ssum = toList (sShelves ssum)


