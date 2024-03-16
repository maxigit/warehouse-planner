module WarehousePlanner.Brick.App
(
whMain
)
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Summary as S
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Brick.RenderBar
import WarehousePlanner.Exec (execWH)
import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Graphics.Vty.Attributes as V
import qualified Graphics.Vty.Input.Events as V
import Control.Monad.State (get, gets, modify)
import Data.List.NonEmpty(nonEmpty, NonEmpty)
import qualified Brick.Widgets.List as B
import qualified Data.Foldable as F
import Brick.Widgets.Center as B

type Resource = Text
type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode | EPrevMode

initState :: forall s . WH (AppState) s
initState = do
  runs <- gets shelfGroup
  shelvesSummary <- traverseRuns findShelf runs >>= makeRunsSummary 
  let toL :: forall a . ShelvesSummary NonEmpty a -> SumZip a
      toL ShelvesSummary{..} = let
          sumZip = B.listMoveTo 0 $ B.list sName (fromList $ F.toList sShelves) 1 
          in ShelvesSummary{sShelves=sumZip,..}
  let asShelvesSummary = fromRuns toL toL toL $ mapRuns (\s -> s { sShelves = B.list (S.sName s) mempty 1   } )  shelvesSummary
  let asViewMode = ViewSummary SVVolume
  return AppState{..}


  

whApp :: WHApp
whApp =
  let
      app = B.App {..}
      appDraw = \s -> 
              let main = case asViewMode s of
                           -- ViewSummary smode -> [ B.vBox $ map (shelfSummaryToBar VerticalBar smode) (asShelvesSummary s) ]
                           ViewSummary smode ->   B.hBox $ renderSummaryAsList "Runs" smode (asShelvesSummary s)
                                                         : B.vBorder
                                                         : case B.listSelectedElement (sShelves $ asShelvesSummary s) of
                                                                Nothing -> []
                                                                Just (_, run) -> [ renderSummaryAsList "Run" smode ( run)
                                                                            ]
                  mainRun = case asViewMode s of 
                              ViewSummary smode -> renderHorizontalRun smode (currentRun s)
              in  [ B.vBox [ mainRun
                           , B.hBorder
                           , main
                           , B.hBorder
                           , renderStatus s
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
  B.VtyEvent (V.EvKey (V.KChar 'm') [] ) -> handleWH ENextMode
  B.VtyEvent (V.EvKey (V.KChar 'M') [] ) -> handleWH EPrevMode
  B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
  B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl] ) -> B.halt
  B.VtyEvent ev -> do
             l0 <- gets asShelvesSummary
             l <- B.nestEventM' (sShelves l0)
                                (B.handleListEventVi B.handleListEvent ev
                                )
             modify (\s -> s { asShelvesSummary  = l0 { sShelves = l } } )
             return ()

  _ -> B.resizeOrQuit ev
 
handleWH ENextMode = modify nextMode
handleWH EPrevMode = modify prevMode

  
nextMode :: AppState -> AppState
nextMode state = case asViewMode state of
         ViewSummary vmode ->  state { asViewMode = ViewSummary $ succ' vmode }
         _ -> state
  
prevMode :: AppState -> AppState
prevMode state = case asViewMode state of
         ViewSummary vmode ->  state { asViewMode = ViewSummary $ pred' vmode }
         _ -> state


-- * 
renderSummaryAsList name smode ssum@ShelvesSummary{..} =
  B.renderList (\selected e -> B.hBox $ shelfSummaryToAllBars e
                                      : B.str (if selected then "*" else " ")
                                      : B.txt (S.sName e)
                                      : B.str " "
                                      -- : map (shelfSummaryToBar VerticalBar smode) (F.toList $ S.sShelves e)
                                      -- : (intersperse (B.str "|") $ map renderS (F.toList $ S.sShelves e) )
                                      : (  map (renderS smode) (F.toList $ S.sShelves e) )
               )
               True
               (sShelves {B.listName = name })
  where -- renderS = shelfSummaryToAllBars 

-- renderStatus :: AppState -> Widgets
renderStatus state@AppState{..} = let
  mode = case asViewMode of
          ViewSummary v -> B.str (show v)
  legend = B.hBox [ B.withAttr (percToAttrName r 0) (B.str [eigthV i]) | i <- [0..8] , let r = fromIntegral i / 8 ]
  current = B.txt $ maybe "" (sName . snd) $ B.listSelectedElement (sShelves asShelvesSummary)
  content = renderWithStyleName (currentRun state)
  in B.vLimit 1 $ B.hBox $ [current
                           , content
                           , B.center mode
                           , B.padLeft B.Max legend]
             
