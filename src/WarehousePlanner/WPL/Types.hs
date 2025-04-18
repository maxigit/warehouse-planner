module WarehousePlanner.WPL.Types
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Selector(printBoxSelector, printShelfSelector)
import WarehousePlanner.Expr (Expr)
import Data.List.NonEmpty as NE


data Command = Move { cSource :: Maybe BoxSelector
                    , cPartitionMode :: Maybe PartitionMode
                    , cOrientationStrategies :: [OrientationStrategy]
                   , cDest   :: CSelector ShelfSelector
                   , cExitMode  :: ExitMode 
                   }
             | Tag { cTagOps :: [Tag'Operation] }
             | TagFor (CSelector BoxSelector) [Tag'Operation] Statement
             | ToggleTags { cTagOps :: [Tag'Operation] } -- tag included and "un"tag excluded
             | TagShelves { cTagOps :: [Tag'Operation] }
             | SelectBoxes (CSelector BoxSelector)
             | SelectBoxRanges RangeBoundary (CSelector BoxSelector)
             | SelectShelves (CSelector ShelfSelector)
             | TagAndMove Text [OrientationStrategy] -- as in :TAM:
             | Delete 
             | SetPartitionMode PartitionMode
             | SetOrientationStrategies (Maybe ShelfSelector) [OrientationStrategy]
             | AddOrientationStrategies (Maybe ShelfSelector) [OrientationStrategy]
             | TraceCount Text
             | TraceBoxes Text (Maybe Text)
             | TraceShelves Text
             | TraceOrientations Text
             | SetNoEmptyBoxes Bool
             | SetNoEmptyShelves Bool
             | AssertBoxes Bool Text
             | AssertShelves Bool Text
             | ResizeShelfFull (CSelector ShelfSelector) Statement
             | ResizeShelf (CSelector ShelfSelector) (Expr Text) (Expr Text) (Expr Text) Statement
             | ResizeBox BoxMode (CSelector BoxSelector) Statement
             | SplitShelf (CSelector ShelfSelector) (Maybe (CSelector BoxSelector)) [Expr Text] [Expr Text] [Expr Text] Statement
             | SwapBoxes (CSelector BoxSelector) (Maybe Text) [Text]
     deriving (Show, Eq)
     {-
       deriving Eq
instance Show Command where show = showCommand
showCommand = \case
      Move{..} -> "Move " <> maybe "∅" showBoxSelector cSource <> " " <>  showCSelector showShelfSelector cDest
      Tag{..} -> "Tag " <> show cTagOps
      ToggleTags{..} -> "ToggleTag " <> show cTagOps
      SelectBoxes s -> showCSelector showBoxSelector s
      SelectShelves s -> showCSelector showShelfSelector s
      TagAndMove txt ors -> "TagAndMove" <> show txt <> " " <> show ors
      Delete -> "Delete"
      TraceCount descr -> "TraceCount " <> show descr
      SetPartitionMode pmode -> "Set PMode " <> show pmode
      SetOrientationStrategies os -> "Orientations " <> show os
      _ -> "????"
      -}
       
data RangeBoundary = Before | After | From | Upto
  deriving (Eq, Show, Ord, Enum, Bounded)
      
data BoxMode = MaxDimension | MinDimension | FirstDimension
  deriving (Eq, Show, Ord, Enum, Bounded)

showBoxSelector = unpack . printBoxSelector
showShelfSelector s = "<" <> (unpack $ printShelfSelector s) <> ">"
            
data Statement = Action Command
               | Cases (NonEmpty Case)
               | Ors (NonEmpty Statement)
               | Then Statement Statement
               | PassThrought Statement
               | ForeachShelf Statement
               | ForeachBox (CSelector BoxSelector) Statement
               | ShelfCases (NonEmpty ShelfCase)
               | ForeachDo Statement (NonEmpty Statement)
               | PrettyPrint Text Statement
     deriving (Show, Eq)
     
data Case = Case Statement (Maybe Statement)
    deriving (Show, Eq)

data ShelfCase = ShelfCase Statement (Maybe Statement)
    deriving (Show, Eq)


-- | Context selector, a bit more than a selector
-- as it allows accessing parent for example
data CSelector  s = CSelector s
                  | SwapContext
                  | Parent 
                  | Root
                  | CStatement Statement
                  | CUseContext
                  | CSelectorAnd (CSelector s) (CSelector s)
     deriving (Eq, Show)
     
showCSelector shows sel = case sel of
    CSelector s -> shows s
    SwapContext -> "<SwapContext>"
    Parent -> "<Parent>"
    Root -> "<Root>"
    CStatement stmt -> "<" <> show stmt <> ">"
    CUseContext -> "<ctxt>"
    CSelectorAnd s c -> "(" <> showCSelector shows s <> " " <> showCSelector shows c <> ")"

     

