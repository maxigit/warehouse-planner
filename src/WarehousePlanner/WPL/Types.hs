module WarehousePlanner.WPL.Types
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Selector(printBoxSelector, printShelfSelector)
import Data.List.NonEmpty as NE


data Command = Move { cSource :: Maybe BoxSelector
                   , cDest   :: CSelector ShelfSelector
                   }
             | Tag { cTagOps :: [Tag'Operation] }
             | ToggleTags { cTagOps :: [Tag'Operation] } -- tag included and "un"tag excluded
             | SelectBoxes (CSelector BoxSelector)
             | SelectShelves (CSelector ShelfSelector)
             | TagAndMove Text [OrientationStrategy] -- as in :TAM:
             | Delete 
             | TraceCount Text
     deriving (Show, Eq)
     -- deriving Eq
-- instance Show Command where show = showCommand
showCommand = \case
      Move{..} -> "Move " <> maybe "∅" showBoxSelector cSource <> " " <>  showCSelector showShelfSelector cDest
      Tag{..} -> "Tag " <> show cTagOps
      ToggleTags{..} -> "ToggleTag " <> show cTagOps
      SelectBoxes s -> showCSelector showBoxSelector s
      SelectShelves s -> showCSelector showShelfSelector s
      TagAndMove txt ors -> "TagAndMove" <> show txt <> " " <> show ors
      Delete -> "Delete"
      TraceCount descr -> "TraceCount " <> show descr
      
showBoxSelector = unpack . printBoxSelector
showShelfSelector s = "<" <> (unpack $ printShelfSelector s) <> ">"
            
data Statement = Action Command
               | Cases (NonEmpty Case)
               | Ors (NonEmpty Statement)
               | Then Statement Statement
               | PassThrought Statement
     deriving (Show, Eq)
     
data Case = Case Statement (Maybe Statement)
    deriving (Show, Eq)



-- | Context selector, a bit more than a selector
-- as it allows accessing parent for example
data CSelector  s = CSelector s
                  | SwapContext
                  | Parent 
                  | Root
                  | CStatement Statement
     deriving (Eq, Show)
     
showCSelector shows sel = case sel of
    CSelector s -> shows s
    SwapContext -> "<SwapContext>"
    Parent -> "<Parent>"
    Root -> "<Root>"
    CStatement stmt -> "<" <> show stmt <> ">"

     

