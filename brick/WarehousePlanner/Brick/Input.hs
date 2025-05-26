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
import Data.Text.Zipper (gotoEOL, insertMany, clearZipper)
import Data.List qualified as List
import WarehousePlanner.Brick.Util(bold_)

                   
                   
selectInput :: InputMode -> InputData -> Input
selectInput imode initial = Input ed imode initial where
  ed = applyEdit gotoEOL $ editorText "input" (Just 1) (idInitial initial)

renderInput :: Input -> Widget Resource
renderInput Input{..} = help <=> (mode <+> ed) where
  mode = case iMode of
          ISelectBoxes -> str "/"
          ISelectShelves -> str "?"
          ISelectProperty -> str "[Prop]"
          ISelectTag -> str "#"
          ISelectShelfProperty -> str "/[Prop]"
  ed = renderEditor renderLines True iEditor
  renderLines  = hBox . map txt 
  help = vBox $ [ str "bo" <+> hl "X" <+> str "sel. "
                , str "c" <+> hl "O" <+> str "ntent "
                , str "p" <+> hl "R" <+> str "op value "
                , str "" <+> hl "S" <+> str "helf "
                , str "she" <+> hl "L" <+> str "f sel "
                , str "st" <+> hl "Y" <+> str "le "
                ]
  hl = withAttr bold_ . str


handleInputEvent ::  BrickEvent Resource e -> EventM Resource Input (Either (Maybe Text) Input)
handleInputEvent ev = do
    input <- get
    let insert' value = 
               let newEd = applyEdit (insertMany value) (iEditor input)
               -- put input { iEditor = newed }
               in input { iEditor = newEd }
        insert field = return $ Right $ insert' (field $ iData input)
        history rot = case rot (idHistory $ iData input) of
                       [] -> return $ Right input 
                       hist@(x:_) -> return $ Right $ input { iData = (iData input) { idHistory = hist }
                                                            , iEditor = applyEdit (insertMany x . clearZipper) (iEditor input)
                                                            }
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
        VtyEvent (V.EvKey ( V.KChar 'n' ) [V.MCtrl]) -> history rotate
        VtyEvent (V.EvKey ( V.KChar 'p' ) [V.MCtrl]) -> history rotateBackward
        _ -> do 
               (newed, _) <- nestEventM (iEditor input) $ handleEditorEvent ev
               -- put input { iEditor = newed }
               return $ Right input { iEditor = newed }



rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

rotateBackward :: [a] -> [a]
rotateBackward [] = []
rotateBackward xs = List.last xs : List.init xs
