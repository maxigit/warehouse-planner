module WarehousePlanner.Brick.Input
( selectInput
, renderInput
, handleInputEvent
)
where

import ClassyPrelude
import Brick
import Brick.Widgets.Edit
-- import Brick.Widgets.Core
import WarehousePlanner.Brick.Types
import Graphics.Vty.Input.Events qualified as V
import Data.Text.Zipper (gotoEOL, insertMany)

                   
                   
selectInput :: InputMode -> InputData -> Input
selectInput imode initial = Input ed imode initial where
  ed = applyEdit gotoEOL $ editorText "input" (Just 1) (idInitial initial)

renderInput :: Input -> Widget Resource
renderInput Input{..} = mode <+> ed where
  mode = case iMode of
          ISelectBoxes -> str "/"
          ISelectShelves -> str "?"
  ed = renderEditor renderLines True iEditor
  renderLines  = hBox . map txt 


handleInputEvent ::  BrickEvent Resource e -> EventM Resource Input (Either (Maybe Text) Input)
handleInputEvent ev = do
    input <- get
    let insert field = do 
               let newEd = applyEdit (insertMany $ field $ iData input) (iEditor input)
               -- put input { iEditor = newed }
               return $ Right input { iEditor = newEd }
    case ev of
        VtyEvent (V.EvKey V.KEsc _) -> return . Left $ Nothing
        VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl] ) -> return . Left $ Nothing
        VtyEvent (V.EvKey V.KEnter _) -> return . Left $ Just $ fromMaybe ""  $ headMay $ getEditContents (iEditor input)
        VtyEvent (V.EvKey ( V.KChar 's' ) [V.MCtrl]) -> insert idShelf
        VtyEvent (V.EvKey ( V.KChar 'r' ) [V.MCtrl]) -> insert idPropertyValue
        VtyEvent (V.EvKey ( V.KChar 'y' ) [V.MCtrl]) -> insert idStyle
        VtyEvent (V.EvKey ( V.KChar 'o' ) [V.MCtrl]) -> insert idContent
        VtyEvent (V.EvKey ( V.KChar 'x' ) [V.MCtrl]) -> insert idBoxSelector
        VtyEvent (V.EvKey ( V.KChar 'l' ) [V.MCtrl]) -> insert idShelfSelector
        -- VtyEvent (V.EvKey $ V.KChar 'n' Ctrl) -> history
        -- VtyEvent (V.EvKey $ V.KChar 'p' Ctrl) -> history
        _ -> do 
               (newed, _) <- nestEventM (iEditor input) $ handleEditorEvent ev
               -- put input { iEditor = newed }
               return $ Right input { iEditor = newed }



