module WarehousePlanner.Brick.Types
( AppState(..)
, ViewMode(..)
, Layout(..)
, SummaryView(..)
, AdjustShelveMode(..) 
, DepthMode(..)
, BoxOrder(..)
, BarDirection(..)
, SumVec, sDetailsList
, selectFromSumVec
, currentRun, currentBay, currentShelf, currentBox, currentBoxHistory
, selectedPropValue, currentPropValue
, sPropValues, sShelfPropValues
, SummaryExtra(..)
, asHistoryRange
, asViewMode
, asCurrentEvent
, HighlightStatus(..)
, Input(..), InputMode(..), InputData(..)
, Resource
, Selection(..)
)
where

import ClassyPrelude
import WarehousePlanner.Summary
import WarehousePlanner.Type
import Data.Vector qualified as V
import Data.Foldable qualified as F
import Brick.Widgets.Edit(Editor)

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
     deriving (Show, Eq, Ord, Enum, Bounded)
     
data ViewMode = ViewSummary SummaryView
              | ViewHistory
              -- | ViewSplitShelves (Shelf s -> Bool)
              -- | ViewSplitBoxes (Box s -> Bool)
              -- 
              deriving (Show, Eq, Ord)
              
data Layout = DoubleRun
            | PropRun
            | TitleRun
            | DoubleRunCompact
     deriving (Show, Eq, Ord, Enum, Bounded)

data AdjustShelveMode = AllShelves
                      | SelectedShelves
                      | UnselectedShelves
                      | SelectedShelvesFirst
                      | SelectedBoxes
            deriving (Show, Eq, Ord, Enum, Bounded)
data DepthMode = DMSlot -- ^ Displays all boxes in the same slot
               | DMWide -- ^ Displays each depth as a shelf
               | DMDistinct -- ^ Displays distinct values 
               | DMDistinctAndCount -- ^ Displays distinct values withcount
               | DMFirstAndCount
               | DMFirst
     deriving (Show, Eq,Ord, Bounded, Enum)
             

data BoxOrder = BOByName
              | BOByShelve
              | BOByCount
              | BOByVolume
     deriving (Show, Eq, Ord, Enum, Bounded)

type Resource = Text
data InputMode = ISelectBoxes
               | ISelectShelves
               | ISelectProperty 
               | ISelectTag
               | ISelectShelfProperty
     deriving (Show, Eq, Ord, Enum, Bounded)

data Input = Input { iEditor :: Editor Text Resource
                   , iMode :: InputMode
                   , iData  :: InputData
                   }
                   
-- | Values like current boxstyle, shelf etc
-- to insert with special keys when editing input.
data InputData = InputData { idInitial :: Text
                           , idShelf :: Text
                           , idPropertyValue :: Text
                           , idStyle :: Text
                           , idContent :: Text
                           , idBoxSelector :: Text
                           , idShelfSelector :: Text
                           , idHistory :: [Text]
                           }
   deriving (Show, Eq)
-- | Summary with a "list" with current position
type SumVec = ShelvesSummary SummaryExtra Vector

data SummaryExtra = SummaryExtra 
     { sePropValues :: Map Text Summary
     , seShelfPropValues :: Map Text Summary
     , seEvents :: Map Event (DiffStatus (Set Text)) -- ^ event which happen to items 
     , seBoxHLStatus :: HighlightStatus
     , seShelfHLStatus :: HighlightStatus
     }
      deriving (Show)
      
instance Semigroup SummaryExtra where
  e1 <> e2 = SummaryExtra (unionWith (<>) (sePropValues e1) (sePropValues e2))
                          (unionWith (<>) (seShelfPropValues e1) (seShelfPropValues e2))
                          (unionWith (<>) (seEvents e1) (seEvents e2))
                          (seBoxHLStatus e1 <> seBoxHLStatus e2)
                          (seShelfHLStatus e1 <> seShelfHLStatus e2)

sPropValues :: ShelvesSummary SummaryExtra a b -> Map Text Summary
sPropValues = sePropValues . sExtra

sShelfPropValues :: ShelvesSummary SummaryExtra a b -> Map Text Summary
sShelfPropValues = seShelfPropValues . sExtra

data AppState = AppState
     { -- asViewMode  :: ViewMode
       asLayout :: Layout
     , asSummaryView :: SummaryView
     , asDisplayHistory :: Bool
     , asDisplayDetails :: Bool
     , asAdjustedShelvesMode :: AdjustShelveMode
     , asShelvesSummary :: Runs SumVec (SumVec  (ZHistory (Box RealWorld)))
     , asCurrentRun :: Int
     , asCurrentBay :: Int
     , asCurrentShelf :: Int
     , asCurrentBox :: Int
     ---------
     , asProperty :: Maybe Text
     , asShelfProperty :: Maybe Text
     , asSelectedPropValue :: Maybe Text
     , asCurrentPropValue :: Int 
     , asCurrentRunPropValues :: Vector (Text, Summary)
     , asPropertyAsGradient :: Maybe Bool -- Just True for all False for current run
     , asPropertyGlobal :: Bool -- Displall property values (all runs)
     , asShowSelected :: Bool
     ------ deal with multikey mapping
     , asSubmap :: Maybe Char
     , asDisplayMainHelp :: Bool
     , asBoxOrder :: (BoxOrder, Bool)
     , asWarehouse :: Warehouse RealWorld 
     , asTitle :: String
     , asDiffEvent :: Event -- ^ Event to show diff with
     , asNavigateCurrent :: Bool -- ^ navigate current event instead of diff event
     , asNavigateWithPrevious ::  Bool -- ^ if true, make diff = previous of 
     , asNavigateFollowsBox :: Bool -- ^ If true , follows the curret box
     , asDebugShowDiffs :: Bool
     , asInput :: Maybe Input
     , asInputHistory :: Map InputMode [Text]
     , asBoxSelection :: Maybe (Selection BoxSelector (BoxId RealWorld))
     , asShelfSelection :: Maybe (Selection ShelfSelector Text)
     , asDepthMode :: DepthMode -- ^ How to boxes in the same slot with different depth
     , asCollapseHeight :: Bool -- ^ collapse boxes downward (as in with gravity)
     , asReload :: (IO (Either Text (Warehouse RealWorld)) -- to reload
                   , IO () -- how to send a event to start the reload
                   )
     , asReloading :: Bool
     , asToday :: Day
     }
     
data Selection sel a = 
         Selection { sText :: Text
                   , sSelector :: sel
                   , sSelected :: Set a
                   }
     deriving Show

asCurrentEvent :: AppState -> Event
asCurrentEvent = whCurrentEvent . asWarehouse

asHistoryRange :: AppState -> HistoryRange
asHistoryRange app = HistoryRange{..} where
               hrCurrent = whCurrentEvent (asWarehouse app)
               hrToDiff = asDiffEvent app
     
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

selectedPropValue :: AppState -> Maybe Text
selectedPropValue state | asShowSelected state == False = Nothing
selectedPropValue state | asViewMode state == ViewHistory = Nothing
selectedPropValue state@AppState{..} = asSelectedPropValue <|> currentPropValue state

currentPropValue :: AppState -> Maybe Text
currentPropValue = fmap fst . currentPropValue'Sum
currentPropValue'Sum AppState{..} =
  case length asCurrentRunPropValues of 
       0 -> Nothing
       l -> asCurrentRunPropValues V.!? (asCurrentPropValue `mod` l)

currentBoxHistory :: AppState -> (ZHistory1 Box RealWorld)
currentBoxHistory app = case currentShelf app of
                    v | null (sDetails $ v) -> ZHistory mempty mempty
                    detail -> selectFromSumVec (asCurrentBox app) $ detail

currentBox :: AppState -> Maybe (Box RealWorld)
currentBox = zCurrent . currentBoxHistory



-- * Highlight Status
data HighlightStatus = HighlightStatus
         { hsHighlighted :: Int
         , hsSelected :: Int
         , hsCurrent :: Bool
         }
     deriving (Show, Eq)
     
instance Semigroup HighlightStatus where
   a <> b = HighlightStatus 
              { hsHighlighted = hsHighlighted a + hsHighlighted b
              , hsSelected = hsSelected a + hsSelected b
              , hsCurrent = hsCurrent a || hsCurrent b
              }
 
instance Monoid HighlightStatus where
  mempty = HighlightStatus { hsHighlighted = 0
                           , hsSelected = 0
                           , hsCurrent = False
                           }
