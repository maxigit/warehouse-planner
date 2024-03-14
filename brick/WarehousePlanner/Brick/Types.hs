module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, SummaryView(..)
, BarDirection(..)
, SumZip
)
where

import ClassyPrelude
import WarehousePlanner.Summary
import WarehousePlanner.Type
import qualified Brick.Widgets.List as B

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
     , asShelvesSummary :: Runs SumZip () 
     }
