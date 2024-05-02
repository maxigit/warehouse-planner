module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, SummaryView(..)
, BoxOrder(..)
, BarDirection(..)
, SumVec, sDetailsList
, selectFromSumVec
, currentRun, currentBay, currentShelf, currentBox
, selectedStyle, currentStyle
, sStyles
, SummaryExtra(..)
, HistoryRange
, asHistoryRange
, asViewMode
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
              | ViewHistory
              -- | ViewSplitShelves (Shelf s -> Bool)
              -- | ViewSplitBoxes (Box s -> Bool)
              -- 
              deriving (Show, Eq)
data BoxOrder = BOByName
              | BOByShelve
              | BOByCount
              | BOByVolume
     deriving (Show, Eq, Enum, Bounded)

-- | Summary with a "list" with current position
type SumVec = ShelvesSummary SummaryExtra Vector

data SummaryExtra = SummaryExtra 
     { seStyles :: Map Text Summary
     , seEvents :: Map Event (DiffStatus (Set Text)) -- ^ event which happen to items 
     }
      deriving (Show)
      
instance Semigroup SummaryExtra where
  e1 <> e2 = SummaryExtra (unionWith (<>) (seStyles e1) (seStyles e2))
                          (seEvents e1 <> seEvents e2)
  

sStyles :: ShelvesSummary SummaryExtra a b -> Map Text Summary
sStyles = seStyles . sExtra

data AppState = AppState
     { -- asViewMode  :: ViewMode
     asSummaryView :: SummaryView
     , asDisplayHistory :: Bool
     , asShelvesSummary :: Runs SumVec (SumVec  (History Box RealWorld))
     , asCurrentRun :: Int
     , asCurrentBay :: Int
     , asCurrentShelf :: Int
     , asCurrentBox :: Int
     ---------
     , asSelectedStyle :: Maybe Text
     , asCurrentStyle :: Int 
     , asCurrentRunStyles :: Vector (Text, Summary)
     ------ deal with multikey mapping
     , asLastKeys :: [Char]
     , asBoxOrder :: BoxOrder 
     , asWarehouse :: Warehouse RealWorld 
     , asTitle :: String
     , asDiffEvent :: Event -- ^ Event to show diff with
     , asDiffEventStack :: [Event] -- ^ history of selected events (second part of a zipper)
     }
     
asHistoryRange :: AppState -> HistoryRange
asHistoryRange app = (asDiffEvent app, whCurrentEvent (asWarehouse app))
     
asViewMode :: AppState -> ViewMode
asViewMode app = if asDisplayHistory  app
                 then ViewHistory
                 else ViewSummary $ asSummaryView app

selectFromSumVec :: Int -> SumVec a -> a
selectFromSumVec i ShelvesSummary{sDetails} = sDetails V.! (i `min` (length sDetails - 1))

currentRun :: AppState -> Run SumVec (Bay SumVec _)
currentRun state = selectFromSumVec (asCurrentRun state) (asShelvesSummary state)

-- currentBay :: AppState -> Bay SumVec _a
currentBay state = selectFromSumVec (asCurrentBay state) (currentRun state)

currentShelf :: AppState -> _a
currentShelf state = selectFromSumVec (asCurrentShelf state) (currentBay state)

sDetailsList :: Foldable f => ShelvesSummary e f a -> [a]
sDetailsList ssum = F.toList (sDetails ssum)

selectedStyle :: AppState -> Maybe Text
selectedStyle state@AppState{..} = asSelectedStyle <|> currentStyle state

currentStyle :: AppState -> Maybe Text
currentStyle = fmap fst . currentStyle'Sum
currentStyle'Sum AppState{..} =
  case length asCurrentRunStyles of 
       0 -> Nothing
       l -> asCurrentRunStyles V.!? (asCurrentStyle `mod` l)

currentBoxHistory :: AppState -> Maybe (History Box RealWorld)
currentBoxHistory app = case currentShelf app of
                    v | null (sDetails $ v) -> Nothing
                    detail -> Just $ selectFromSumVec (asCurrentBox app) $ detail

currentBox :: AppState -> Maybe (Box RealWorld)
currentBox = fmap fromHistory . currentBoxHistory



-- * History
type HistoryRange = (Event, Event)
