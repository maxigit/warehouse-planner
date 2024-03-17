module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, SummaryView(..)
, BarDirection(..)
, SumZip
, currentRun, currentBay, currentShelf
)
where

import ClassyPrelude
import WarehousePlanner.Summary
import WarehousePlanner.Type
import qualified Brick.Widgets.List as B
import Data.Maybe (fromJust)

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
type SumZip = ShelvesSummary (B.GenericList Text Vector)
data AppState = AppState
     { asViewMode  :: ViewMode
     , asShelvesSummary :: Runs SumZip (SumZip (Box RealWorld)) 
     }
     
listSelectedElement :: B.GenericList Text Vector a -> a
listSelectedElement glist = 
  snd case B.listSelectedElement glist of
    Nothing -> fromJust $ B.listSelectedElement $ B.listMoveTo 0 glist
    Just e -> e

currentRun :: AppState -> Run SumZip (Bay SumZip _)
currentRun state = listSelectedElement (sShelves $ asShelvesSummary state)

currentBay :: AppState -> Bay SumZip _a
currentBay = listSelectedElement . sShelves . currentRun 

currentShelf :: AppState -> _a
currentShelf = listSelectedElement . sShelves . currentBay
