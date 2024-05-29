module WarehousePlanner.WPL.Types
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Selector(printBoxSelector, printShelfSelector)
import Data.List.NonEmpty as NE


data Command = Move { cSource :: Maybe BoxSelector
                   , cDest   :: Maybe ShelfSelector
                   }
             | Tag { cTagOps :: [Tag'Operation] }
             | ToggleTags { cTagOps :: [Tag'Operation] } -- tag included and "un"tag excluded
             | SelectBoxes BoxSelector
             | SelectShelves ShelfSelector 
            -- | Tag { source :: Text, tags }
     -- deriving (Show, Eq)
     deriving Eq
instance Show Command where show = showCommand
showCommand = \case
      Move{..} -> "Move " <> maybe "∅" showBoxSelector cSource <> " " <>  maybe "∅" showShelfSelector cDest
      Tag{..} -> "Tag " <> show cTagOps
      ToggleTags{..} -> "ToggleTag " <> show cTagOps
      SelectBoxes s -> showBoxSelector s
      SelectShelves s -> showShelfSelector s
      
showBoxSelector = unpack . printBoxSelector
showShelfSelector s = "<" <> (unpack $ printShelfSelector s) <> ">"
            
data Statement = Action Command
               | Cases (NonEmpty Case)
               | Ors (NonEmpty Statement)
               | Then Statement Statement
     deriving (Show, Eq)
     
data Case = Case Statement (Maybe Statement)
    deriving (Show, Eq)


