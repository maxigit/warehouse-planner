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
import Data.List.NonEmpty(nonEmpty, NonEmpty)
import qualified Brick.Widgets.List as B
import qualified Data.Foldable as F

type Resource = Text
type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode

initState :: forall s . WH (AppState) s
initState = do
  runs <- gets shelfGroup
  shelvesSummary <- traverseRuns findShelf runs >>= makeRunsSummary 
  let toL :: forall a . ShelvesSummary NonEmpty a -> SumZip a
      toL ShelvesSummary{..} = let
          sumZip = B.listMoveTo 0 $ B.list sName (fromList $ F.toList sShelves) 1 
          in ShelvesSummary{sShelves=sumZip,..}
  let asShelvesSummary = fromRuns toL toL toL $ mapRuns (const ())  shelvesSummary
  let asViewMode = ViewSummary SVVolume
  return AppState{..}


  

whApp :: WHApp
whApp =
  let
      app = B.App {..}
      appDraw = \s -> case asViewMode s of
                           -- ViewSummary smode -> [ B.vBox $ map (shelfSummaryToBar VerticalBar smode) (asShelvesSummary s) ]
                           ViewSummary smode -> [ B.hBox $ renderSummaryAsList "Runs" smode (asShelvesSummary s)
                                                         : case B.listSelectedElement (sShelves $ asShelvesSummary s) of
                                                                Nothing -> []
                                                                Just (_, run) -> [ renderSummaryAsList "Run" smode ( run)
                                                                            ]
                                                ]
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
  B.VtyEvent (V.EvKey (V.KChar ' ') [] ) -> handleWH ENextMode
  B.VtyEvent ev -> do
             l0 <- gets asShelvesSummary
             l <- B.nestEventM' (sShelves l0)
                                (B.handleListEventVi B.handleListEvent ev
                                )
             modify (\s -> s { asShelvesSummary  = l0 { sShelves = l } } )
             return ()

  _ -> B.resizeOrQuit ev
 
handleWH ENextMode = modify nextMode

  
nextMode :: AppState -> AppState
nextMode state = case asViewMode state of
         ViewSummary vmode ->  state { asViewMode = ViewSummary $ succ' vmode }
         _ -> state
  


-- * 
renderSummaryAsList name smode ShelvesSummary{..} =
  B.renderList (\_ e -> shelfSummaryToBar VerticalBar smode e)
               True
               (sShelves {B.listName = name })

