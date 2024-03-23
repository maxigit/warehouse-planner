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
import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Input.Events qualified as V
import Control.Monad.State (get, gets, modify)
import Data.List.NonEmpty(nonEmpty, NonEmpty)
import Brick.Widgets.List qualified as B
import Data.Foldable qualified as F
import Brick.Widgets.Center qualified as B
import Brick.Widgets.Table qualified as B
import Data.List (cycle)
import Data.Map qualified as Map
import WarehousePlanner.Brick.Table
import Data.Vector qualified as V

type Resource = Text
type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode
             | EPrevMode
             | ENextRun
             | EPrevRun
             | ENextBay
             | EPrevBay
             | ENextShelf
             | EPrevShelf
             | ENextBox
             | EPrevBox
             | EFirstRun
             | ELastRun
             | EFirstBay
             | ELastBay
             | ESelectCurrentStyle
             | ENextStyle
             | EPreviousStyle

initState :: WH (AppState) RealWorld
initState = do
  runs <- gets shelfGroup
  shelvesSummary <- traverseRuns findShelf runs
                 >>= makeRunsSummary
                 >>= traverseRuns findBoxes
  let toL :: forall a . ShelvesSummary NonEmpty a -> SumVec a
      toL ssum@ShelvesSummary{..} = let
          sumZip = fromList $ sDetailsList ssum
          in ShelvesSummary{sDetails=sumZip,..}
  let asShelvesSummary = fromRuns toL toL toL
                       $ mapRuns (\s -> s { sDetails = fromList $ sDetails s }
                                     )  shelvesSummary
  let asSummaryView = SVVolume
  return . runUpdated
         $ AppState{ asCurrentRun=0, asCurrentBay = 0, asCurrentShelf = 0
                 , asSelectedStyle = Nothing, asCurrentStyle = 0, asCurrentRunStyles = mempty
                 , ..}
  where findBoxes ShelvesSummary{sDetails=shelves,..} = do
            let boxIds = concatMap (toList . _shelfBoxes) $ toList shelves
            sDetails <- mapM findBox boxIds
            return ShelvesSummary{..}


whApp :: _ -> WHApp
whApp extraAttrs =
  let
      app = B.App {..}
      appDraw = \s@AppState{..} -> 
              let main = case () of
                           -- ViewSummary smode -> [ B.vBox $ map (shelfSummaryToBar VerticalBar smode) (asShelvesSummary s) ]
                          ()  ->
                                       B.hBox $ B.hLimit 30 (runsSideBar s)
                                              : B.vBorder
                                              : B.hLimit 30 (stylesSideBar s)
                                              : B.vBorder
                                              -- : case B.listSelectedElement (sDetails $ asShelvesSummary s) of
                                              -- : case currentBay s of
                                              --        -- Nothing -> []
                                              --        -- current -> [ (B.hBox 
                                              --        --                . map B.renderTable
                                              --        --                . shelfSummaryToTable  (const B.emptyWidget) -- (B.vBox . map renderBoxOrientation)
                                              --        --                )
                                              --        --                $ current]
                                              --        -- current -> [ B.hBox . map B.renderTable $ shelfSummaryToTable (B.vBox. map renderBoxOrientation) current ]
                                              --        current -> [ B.renderTable $ baySummaryToTable (B.vBox. map renderBoxOrientation) current ]
                                              --             -- [ renderSummaryAsList "Run" smode ( run) ]
                                              : [ B.vBox $ (map B.hBox) [ map (B.padTop B.Max . B.renderTable . baySummaryToTable (B.vBox . map renderBoxOrientation)) (F.toList . sDetails $ currentRun s)
                                                         , map (B.padTop B.Max . B.renderTable . baySummaryToTable (B.vBox . map renderBoxContent)) (drop asCurrentBay $ sDetailsList $ currentRun s)
                                                         ]
                                              ]
                  mainRun = B.emptyWidget -- renderHorizontalRun asSummaryView (currentRun s)
              in  [ B.vBox [ mainRun
                           , debugShelf s
                           , B.hBorder
                           , main
                           , B.hBorder
                           , renderStatus s
                           ]
                  ]
      appChooseCursor = B.neverShowCursor
      appHandleEvent = whHandleEvent
      appAttrMap state = B.attrMap V.defAttr $ generateLevelAttrs  <> extraAttrs state
      appStartEvent = return ()
  in app
  
whMain :: Warehouse RealWorld -> IO ()
whMain wh = do
  state0 <- execWH wh initState
  -- to avoid styles to have the same colors in the same shelf
  -- we sort them by order of first shelves
  let style'shelfs = [ (style, sName shelfSum)
                    | run <- sDetailsList (asShelvesSummary state0)
                    , bay <- sDetailsList run
                    , shelfSum <- sDetailsList bay
                    , style <- keys (sStyles shelfSum)
                    ]
  let styles = reverse $ map fst style'shelfs
      attrs state = selectedAttr
            : zipWith (\style attr -> (makeStyleAttrName style, reverseIf (Just style == selectedStyle state) attr ))
                      styles
                      (cycle defaultStyleAttrs)
      (selected_, selectedAttrs_) = selectedAttr
  void $ B.defaultMain (whApp attrs) state0

reverseIf :: Bool -> V.Attr -> V.Attr
reverseIf True attr = attr `V.withStyle` V.reverseVideo
reverseIf _ attr = attr


whHandleEvent :: B.BrickEvent Resource WHEvent -> B.EventM Resource AppState ()
whHandleEvent ev = case ev of 
  B.AppEvent e -> handleWH e
  B.VtyEvent (V.EvKey (V.KChar 'm') [] ) -> handleWH ENextMode
  B.VtyEvent (V.EvKey (V.KChar 'M') [] ) -> handleWH EPrevMode
  B.VtyEvent (V.EvKey (V.KChar 'j') [] ) -> handleWH ENextRun
  B.VtyEvent (V.EvKey (V.KChar 'k') [] ) -> handleWH EPrevRun
  B.VtyEvent (V.EvKey (V.KChar 'J') [] ) -> handleWH ENextShelf
  B.VtyEvent (V.EvKey (V.KChar 'K') [] ) -> handleWH EPrevShelf
  B.VtyEvent (V.EvKey (V.KChar 'l') [] ) -> handleWH ENextBay
  B.VtyEvent (V.EvKey (V.KChar 'h') [] ) -> handleWH EPrevBay
  B.VtyEvent (V.EvKey (V.KChar 'g') [] ) -> handleWH EFirstRun
  B.VtyEvent (V.EvKey (V.KChar 'G') [] ) -> handleWH ELastRun
  B.VtyEvent (V.EvKey (V.KChar '^') [] ) -> handleWH EFirstBay
  B.VtyEvent (V.EvKey (V.KChar '$') [] ) -> handleWH ELastBay
  B.VtyEvent (V.EvKey (V.KEnter) [] ) -> handleWH ESelectCurrentStyle
  B.VtyEvent (V.EvKey (V.KChar 'j') [V.MCtrl] ) -> handleWH ENextStyle
  B.VtyEvent (V.EvKey (V.KChar 'k') [V.MCtrl ] ) -> handleWH EPreviousStyle
  B.VtyEvent (V.EvKey (V.KChar '>') [] ) -> handleWH ENextStyle
  B.VtyEvent (V.EvKey (V.KChar '<') [] ) -> handleWH EPreviousStyle
  B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
  B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
  B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
  B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl] ) -> B.halt
  _ -> B.resizeOrQuit ev
 
handleWH = \case 
         ENextMode -> modify nextMode
         EPrevMode -> modify prevMode
         --
         ENextRun -> modify \s -> runUpdated s { asCurrentRun = nextOf (asCurrentRun s) (asShelvesSummary s) }
         ENextBay -> modify \s -> s { asCurrentBay = nextOf (asCurrentBay s) (currentRun s) }
         ENextShelf -> modify \s -> s { asCurrentShelf = nextOf (asCurrentShelf s) (currentBay s) }
         -- ENextBox -> modify \s -> s { asCurrentBox = nextOf (asCurrentBox s) (currentShelf s) }
         EPrevRun -> modify \s -> runUpdated s { asCurrentRun = prevOf (asCurrentRun s) (asShelvesSummary s) }
         EPrevBay -> modify \s -> s { asCurrentBay = prevOf (asCurrentBay s) (currentRun s) }
         EPrevShelf -> modify \s -> s { asCurrentShelf = prevOf (asCurrentShelf s) (currentBay s) }
         -- EPrevBox -> modify \s -> s { asCurrentBox = prevOf (asCurrentBox s) (currentShelf s) }
         EFirstRun -> modify \s -> s { asCurrentRun = 0 }
         ELastRun -> modify \s -> s { asCurrentRun = lastOf (asShelvesSummary s) }
         EFirstBay -> modify \s -> s { asCurrentBay = 0 }
         ELastBay -> modify \s -> s { asCurrentBay = lastOf (currentRun s) }
         ESelectCurrentStyle -> modify \s -> s { asSelectedStyle = if currentStyle s == asSelectedStyle s
                                                                   then Nothing
                                                                   else currentStyle s
                                               }
         ENextStyle -> do
                    modify \s -> s { asCurrentStyle = nextOf' (asCurrentStyle s) (asCurrentRunStyles s) }
                    -- handleWH ESelectCurrentStyle
         EPreviousStyle -> do
                    modify \s -> s { asCurrentStyle = prevOf' (asCurrentStyle s) (asCurrentRunStyles s) }
                    -- handleWH ESelectCurrentStyle

         _ -> return ()

  
nextMode :: AppState -> AppState
nextMode state = state { asSummaryView = succ' $ asSummaryView state }
  
prevMode :: AppState -> AppState
prevMode state = state { asSummaryView = pred' $ asSummaryView state }
         
nextOf :: Int -> SumVec a -> Int
nextOf i ShelvesSummary{sDetails} = nextOf' i sDetails
nextOf' i v = min (V.length v - 1) (i+1)

prevOf :: Int -> SumVec a -> Int
prevOf i ShelvesSummary{sDetails} = prevOf' i sDetails
prevOf' i v= max 0 ((min i (V.length v- 1) )  - 1)
                    -- ^ 
                    -- +--- in case i was bigger that the sDetailsector length
                    --      this can happen when changing parents
lastOf :: SumVec a -> Int
lastOf ShelvesSummary{sDetails} = lastOf' sDetails
lastOf' v = V.length v - 1


-- * Post update
-- | update the list of current styles
runUpdated :: AppState -> AppState
runUpdated state@AppState{..} = AppState{asCurrentRunStyles=styles,..} where
    styles = fromList $ keys $ sStyles (currentRun state)
-- *  Run
runsSideBar :: AppState -> B.Widget Text
runsSideBar state@AppState{..} = B.renderTable $ runsToTable (selectedStyle state) asSummaryView asCurrentRun asShelvesSummary 

-- * Styles
stylesSideBar :: AppState -> B.Widget Text
stylesSideBar state@AppState{..} = 
  B.renderTable $ stylesToTable (selectedStyle state) asCurrentStyle asCurrentRunStyles
-- renderStatus :: AppState -> Widgets
renderStatus state@AppState{..} = let
  mode = B.str (show asSummaryView)
  legend = B.hBox [ B.withAttr (percToAttrName r 0) (B.str [eigthV i]) | i <- [0..8] , let r = fromIntegral i / 8 ]
  in B.vLimit 1 $ B.hBox $ [ B.txt (sName $ currentShelf state)  -- current shelf
                           , B.center $ maybe (B.str "∅") styleNameWithAttr (asSelectedStyle ) -- current style
                           , B.center mode
                           , B.padLeft B.Max legend]
             
debugShelf :: AppState -> B.Widget Text
debugShelf state = let
  ssum = currentShelf state
  in B.vBox [ B.hBox $ intersperse (B.str " ") 
                     $ [  B.str (show m)
                       , B.txt $ sName ssum
                       , renderS m ssum
                       , B.txt "shelf" 
                       , B.str . show $ suCount $ sShelvesSummary ssum
                       , B.str . show $ fromSummary m $ sShelvesSummary ssum
                       , B.txt "box" 
                       , B.str . show $ suCount $ sBoxSummary ssum
                       , B.str . show $ fromSummary m $ sBoxSummary ssum
                       ]
          | m <- [minBound .. maxBound ]
          ]
  
