module WarehousePlanner.Brick.App
(
whMain
)
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Summary
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Brick.RenderBar
import WarehousePlanner.Exec (execWH)
import qualified Brick as B
import qualified Graphics.Vty.Attributes as V
import qualified Graphics.Vty.Input.Events as V
import Control.Monad.State (get, gets, modify)
import Data.List.NonEmpty(nonEmpty)

type Resource = ()
type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode

initState :: forall s . WH (AppState) s
initState = do
  groups <- gets shelfGroup
  let idss :: [[ShelfId s]] = map concat (groups) 
  shelvess :: [[Shelf s]] <- mapM (mapM findShelf) idss
  asShelvesSummary <- mapM (fmap erase . summaryFromShelves ) (mapMaybe nonEmpty shelvess)
  let asViewMode = ViewSummary SVVolume
  return AppState{..}
  where erase :: ShelvesSummary a -> ShelvesSummary ()
        erase ShelvesSummary{..} = ShelvesSummary{sShelves = map (const ()) sShelves, ..}


  

whApp :: WHApp
whApp =
  let
      app = B.App {..}
      appDraw = \s -> case asViewMode s of
                           ViewSummary smode -> [ B.vBox $ map (shelfSummaryToBar VerticalBar smode) (asShelvesSummary s) ]
      appChooseCursor = B.neverShowCursor
      appHandleEvent = whHandleEvent
      appAttrMap = const $ B.attrMap V.defAttr generateLevelAttrs 
      appStartEvent = return ()
  in app
  
whMain :: Warehouse RealWorld -> IO ()
whMain wh = do
  state0 <- execWH wh initState
  void $ B.defaultMain whApp state0



whHandleEvent :: B.BrickEvent Resource WHEvent -> B.EventM Resource AppState ()
whHandleEvent ev = case ev of 
  B.AppEvent e -> handleWH e
  B.VtyEvent (V.EvKey (V.KChar _) [] ) -> handleWH ENextMode
  _ -> B.resizeOrQuit ev
 
handleWH ENextMode = modify nextMode

  
nextMode :: AppState -> AppState
nextMode state = case asViewMode state of
         ViewSummary vmode ->  state { asViewMode = ViewSummary $ succ' vmode }
         _ -> state
  



