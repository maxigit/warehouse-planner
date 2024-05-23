module WarehousePlanner.WPL.Types
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Selector(printBoxSelector, printShelfSelector)


data Command = Move { aSource :: Maybe BoxSelector
                   , aDest   :: Maybe ShelfSelector
                   }
             | SelectBoxes BoxSelector
             | SelectShelves ShelfSelector 
            -- | Tag { source :: Text, tags }
     deriving (Eq)
instance Show Command where
   show = \case
      Move{..} -> "Move " <> maybe "∅" showBoxSelector aSource <> " " <>  maybe "∅" showShelfSelector aDest
      SelectBoxes s -> showBoxSelector s
      SelectShelves s -> showShelfSelector s
      
showBoxSelector = unpack . printBoxSelector
showShelfSelector = unpack . printShelfSelector
            
data Statement = Action Command
               | Then Statement Statement -- Chain
               | Else Statement Statement -- Invert context
               | Union Statement Statement
     deriving (Show, Eq)
     


