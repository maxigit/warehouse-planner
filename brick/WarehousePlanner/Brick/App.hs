module WarehousePlanner.Brick.App
(
whMain
)
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Summary as S
import WarehousePlanner.History
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector)
import WarehousePlanner.Base
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Brick.RenderBar
import WarehousePlanner.Brick.BoxDetail
import WarehousePlanner.Brick.Input
import WarehousePlanner.Exec (execWH)
import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Input.Events qualified as V
import Control.Monad.State (gets, get, modify, put, zipWithM)
import Data.List.NonEmpty(NonEmpty(..), (!!))
import Data.Foldable qualified as F
import Brick.Widgets.Center qualified as B
import Brick.Widgets.Table qualified as B
import Data.List (cycle)
import Data.Map qualified as Map
import WarehousePlanner.Brick.Table
import Data.Vector qualified as V
import WarehousePlanner.Display qualified as D
import Diagrams.Backend.Cairo (renderCairo)
import Diagrams (mkSizeSpec2D)
import System.Process (rawSystem)
import Data.Set qualified as Set

type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode
             | EPrevMode
             | EToggleViewHistory
             | EToggleDebugShowDiff
             -- 
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
             -- 
             | ESelectCurrentStyle
             | ENextStyle
             | EPreviousStyle
             -- 
             | ESetBoxOrder BoxOrder
             -- 
             | ENextHLRun
             | EPrevHLRun
             -- 
             | ERenderRun
             --
             | EHistoryEvent HistoryEvent
             | EToggleHistoryNavigation
             --
             | EReload
             -- Input
             | EStartInputSelect InputMode
             -- WH Action
data HistoryEvent = HPrevious
                  | HNext
                  | HParent
                  | HChild
                  | HSkipBackward -- same box
                  | HSkipForward
                  | HPreviousSibling
                  | HNextSibling
                  | HSetCurrent
                  | HResetCurrent
                  | HFirst
                  | HLast
                  | HSwapCurrent
     deriving(Show,Eq)

makeAppShelvesSummary :: WH (Runs SumVec (SumVec  (ZHistory1 Box RealWorld))) RealWorld
makeAppShelvesSummary = do
  runs <- gets shelfGroup
  shelvesSummary <- traverseRuns findShelf runs
                 >>= makeRunsSummary (makeExtra)
                 >>= traverseRuns findBoxes
  let toL :: forall a . ShelvesSummary SummaryExtra NonEmpty a -> SumVec a
      toL ssum@ShelvesSummary{..} = let
          sumZip = fromList $ sDetailsList ssum
          in ShelvesSummary{sDetails=sumZip,..}
  currentEvent <- gets whCurrentEvent
  return $ do
         fromRuns toL toL toL
                       $ mapRuns (\s -> do
                                        s { sDetails = fromList $ map (toZHistory currentEvent) $ sortOn boxOrder  $ sDetails s }
                                     )  shelvesSummary
  where findBoxes ShelvesSummary{sDetails=shelves,..} = do
            let boxIds = concatMap (toList . _shelfBoxes) $ toList shelves
            sDetails <- mapM getBoxHistory boxIds
            return ShelvesSummary{..}
        boxOrder box = let Dimension l w h = boxOffset $ fromHistory box
                       in (w, l, h)
                          -- ^^^^ we swap l and w so that we can nagivate through box of the same depth using "next"
        makeExtra shelf =  do
                        currentEvent <- gets whCurrentEvent
                        boxes <- mapM findBox (_shelfBoxes shelf)
                        let styleMap = Map.fromListWith (<>) [ (boxStyle box, makeBoxesSummary [box])
                                                             | box <- toList boxes
                                                             ]
                        boxHistorys <- mapM getBoxHistory boxes
                        shelfHistory <- getShelfHistory shelf
                        let boxEventMaps = [ computeBoxDiffHistoryFrom $ toZHistory currentEvent history
                                          |  history <- toList boxHistorys
                                          ]
                            shelfEventMap = computeShelfDiffHistoryFrom $ toZHistory currentEvent shelfHistory
                            eventMap = mergeEventMaps currentEvent $ shelfEventMap : boxEventMaps
                        eventsWithName <- flip traverse eventMap \(DiffStatus{..}) -> do
                                               namesIn <-  mapM findShelf $ toList dsBoxIn
                                               namesOut <-  mapM findShelf $ toList dsBoxOut
                                               return (DiffStatus { dsBoxIn=setFromList $ map shelfName namesIn
                                                                  , dsBoxOut=setFromList $ map shelfName namesOut
                                                                  , ..})
                        return $ SummaryExtra styleMap (eventsWithName) mempty mempty

updateHLStatus :: (ZHistory1 Box RealWorld -> HighlightStatus) -> (Text -> HighlightStatus) -> Runs SumVec (SumVec (ZHistory1 Box RealWorld)) -> Runs SumVec (SumVec (ZHistory1 Box RealWorld))
updateHLStatus fbox fshelf theRuns = let
    normBay bay = let 
       shelves = fmap normShelf $ sDetails bay
       in bay { sExtra = ( sExtra bay ) { seBoxHLStatus = foldMap (seBoxHLStatus . sExtra) shelves
                                        , seShelfHLStatus = foldMap (seShelfHLStatus . sExtra) shelves
                                        }
              , sDetails = shelves
              }
    normShelf shelf = shelf { sExtra = (sExtra shelf) { seBoxHLStatus = foldMap fbox (sDetails shelf)
                                                      , seShelfHLStatus = fshelf (sName shelf)} }
    normRun run = let
        bays = fmap normBay $ sDetails run
        in run { sExtra = (sExtra run) { seBoxHLStatus = foldMap (seBoxHLStatus . sExtra) bays
                                       , seShelfHLStatus = foldMap (seShelfHLStatus . sExtra) bays
                                       }
               , sDetails = bays
               }
    runs = fmap normRun $ sDetails theRuns
    in theRuns { sExtra = (sExtra theRuns) { seBoxHLStatus = foldMap (seBoxHLStatus . sExtra) runs
                                           , seShelfHLStatus = foldMap (seShelfHLStatus . sExtra) runs
                                           }
           , sDetails = runs
           }

updateHLState :: AppState -> AppState
updateHLState state = state { asShelvesSummary = updateHLStatus (boxHLStatus state . zCurrentEx) (shelfHLStatus state) (asShelvesSummary state) }

initState :: String -> WH (AppState) RealWorld
initState title = do
  asShelvesSummary <- makeAppShelvesSummary
  let asSummaryView = SVVolume
      asDisplayHistory = False
  warehouse <- get
  return . runUpdated
         $ AppState{ asCurrentRun=0, asCurrentBay = 0, asCurrentShelf = 0, asCurrentBox = 0
                 , asSelectedStyle = Nothing, asCurrentStyle = 0, asCurrentRunStyles = mempty
                 , asBoxOrder = BOByName
                 , asLastKeys = []
                 , asWarehouse = warehouse
                 , asTitle = title
                 , asDiffEvent = whCurrentEvent warehouse
                 , asNavigateCurrent = False
                 , asDebugShowDiffs = False
                 , asInput = Nothing
                 , asBoxSelection = Nothing
                 , asShelfSelection = Nothing
                 , ..}


whApp :: _ -> (IO (Either Text (Warehouse RealWorld))) -> WHApp
whApp extraAttrs reload =
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
                                              : [ B.vBox $ (map B.hBox)
                                                         [ renderRun (\bhistory ->
                                                                         let box = zCurrentEx bhistory
                                                                             rendered =  withHLBoxAttr s renderBoxOrientation box
                                                                         in if asDisplayHistory
                                                                            then historyIndicator rendered
                                                                                                  (\s -> boxShelf box == Just s)
                                                                                                  (asHistoryRange s)
                                                                                                  (computeBoxDiffHistoryFrom bhistory)
                                                                            else rendered
                                                                     )
                                                                     (currentRun s)
                                                         , [B.hBorder]
                                                         , renderRun (withHLBoxAttr s renderBoxContent .  zCurrentEx)
                                                                     (let run = currentRun s
                                                                      in run { sDetails = drop asCurrentBay $ sDetails run }
                                                                     )
                                                         ]
                                              ]
                  mainRun = B.emptyWidget -- renderHorizontalRun asSummaryView (currentRun s)
              in  [ vBoxB [ mainRun
                           , B.vLimit (if asDisplayHistory then 21 else 11) $ hBoxB (debugShelf s :  (pure . boxDetail asWarehouse (asHistoryRange s)) (currentBoxHistory s))
                           , main
                           , maybe (renderStatus s) renderInput asInput
                           ]
                  ]
      appChooseCursor = B.neverShowCursor
      appHandleEvent = whHandleEvent reload
      appAttrMap state = B.attrMap V.defAttr $ generateLevelAttrs  <> extraAttrs state
      appStartEvent = return ()
  in app
  
whMain :: String -> (IO (Either Text (Warehouse RealWorld))) -> IO ()
whMain title reload = do
  whE <- reload
  let wh = case whE of
            Left e -> error (unpack e)
            Right w -> w
  state0 <- execWH wh $ initState title 
  -- to avoid styles to have the same colors in the same shelf
  -- we sort them by order of first shelves
  let style'shelfs = [ (style, sName shelfSum)
                    | run <- sDetailsList (asShelvesSummary state0)
                    , bay <- sDetailsList run
                    , shelfSum <- sDetailsList bay
                    , style <- keys (sStyles shelfSum)
                    ]
  let styles = reverse $ map fst style'shelfs
      attrs _state =
            selectedAttr
            -- : bayNameAN
            : boldAttr : tagNameAttr : virtualTagAttr : specialTagAttr
            : zipWith (\style attr -> (makeStyleAttrName style, attr)) --  reverseIf (Just style == selectedStyle state) attr ))
                      styles
                      (cycle defaultStyleAttrs)
            <> eventAttrs
            <> highlightAttrs
  void $ B.defaultMain (whApp attrs reload) state0

__reverseIf :: Bool -> V.Attr -> V.Attr
__reverseIf True attr = attr `V.withStyle` V.reverseVideo
__reverseIf _ attr = attr


whHandleEvent :: (IO (Either Text (Warehouse RealWorld))) -> B.BrickEvent Resource WHEvent -> B.EventM Resource AppState ()
whHandleEvent reload ev = do
  lasts <- gets asLastKeys
  inputM <- gets asInput
  case ev of 
       _ | Just input <- inputM -> do
             result <- B.nestEventM input $ handleInputEvent ev
             case snd result of
                Left (Just result) -> do
                               case iMode input of
                                    ISelectBoxes -> setBoxSelection result
                                    ISelectShelves -> setShelfSelection result
                               modify \s -> s  { asInput = Nothing }
                Left Nothing -> -- ignore
                         modify \s -> s { asInput = Nothing }
                Right input -> modify \s -> s { asInput = Just input }
       B.AppEvent e -> handleWH e
       B.VtyEvent (V.EvKey (V.KChar 'n') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByName
       B.VtyEvent (V.EvKey (V.KChar 's') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByShelve
       B.VtyEvent (V.EvKey (V.KChar 'c') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByCount
       B.VtyEvent (V.EvKey (V.KChar 'v') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByVolume
       B.VtyEvent (V.EvKey (V.KChar 'h') [] ) | 'z':_ <- lasts  -> handleWH $ EToggleHistoryNavigation
       B.VtyEvent (V.EvKey (V.KChar 'd') [] ) | 'z':_ <- lasts  -> handleWH $ EToggleDebugShowDiff
       B.VtyEvent (V.EvKey (V.KChar 'm') [] ) -> handleWH ENextMode
       B.VtyEvent (V.EvKey (V.KChar 'M') [] ) -> handleWH EPrevMode
       B.VtyEvent (V.EvKey (V.KChar '\t' ) [] ) -> handleWH EToggleViewHistory
       B.VtyEvent (V.EvKey (V.KChar 'j') [] ) -> handleWH ENextRun
       B.VtyEvent (V.EvKey (V.KChar 'k') [] ) -> handleWH EPrevRun
       B.VtyEvent (V.EvKey (V.KChar 'J') [] ) -> handleWH ENextShelf
       B.VtyEvent (V.EvKey (V.KChar 'K') [] ) -> handleWH EPrevShelf
       B.VtyEvent (V.EvKey (V.KChar 'l') [] ) -> handleWH ENextBay
       B.VtyEvent (V.EvKey (V.KChar 'h') [] ) -> handleWH EPrevBay
       B.VtyEvent (V.EvKey (V.KChar 'b') [] ) -> handleWH ENextBox
       B.VtyEvent (V.EvKey (V.KChar 'B') [] ) -> handleWH EPrevBox
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
       B.VtyEvent (V.EvKey (V.KChar '%') [] ) -> do
                  state <- get
                  let currentEvent = whCurrentEvent (asWarehouse state)
                  resultm <- liftIO $ execWH (asWarehouse state) do
                      case (asBoxSelection state , asShelfSelection state) of
                          (Just bs, Just ss) ->  do
                             shelves <- findShelvesByBoxNameAndNames (sSelector ss)
                             newBaseEvent "MOVE %" $ sText bs <> " TO " <> sText ss 
                             leftOver <- moveBoxes ExitLeft PBestEffort SortBoxes (toList $ sSelected bs) shelves
                             zipWithM (updateBoxTags [("error", SetTag )]) leftOver [1..]
                             newWH <- get
                             return $ Just (newWH, bs { sSelected = setFromList $ map boxId leftOver } )
                          _ -> return Nothing
                  case resultm of 
                     Just (newWH, newBoxSelection) -> do 
                          modify \s -> s { asWarehouse = newWH, asBoxSelection = Just newBoxSelection  }
                          setNewWHEvent $ whCurrentEvent newWH
                          modify \s -> s { asDiffEvent = currentEvent, asDisplayHistory = True }
                     Nothing -> return ()
       B.VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl] ) -> do
                  newWHE <- liftIO reload
                  case newWHE of
                       Left e -> error $ unpack e
                       Right newWH -> do
                             modify \s -> s { asWarehouse = newWH }
                             setNewWHEvent $ whCurrentEvent newWH
       B.VtyEvent (V.EvKey (V.KChar '/') _ ) -> handleWH (EStartInputSelect ISelectBoxes)
       B.VtyEvent (V.EvKey (V.KChar '?') _ ) -> handleWH (EStartInputSelect ISelectShelves)
       B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl] ) -> B.halt
       B.VtyEvent (V.EvKey (V.KChar ']') [] ) -> handleWH ENextHLRun
       B.VtyEvent (V.EvKey (V.KChar '[') [] ) -> handleWH EPrevHLRun
       B.VtyEvent (V.EvKey (V.KChar 'v') [] ) -> handleWH ERenderRun
       B.VtyEvent (V.EvKey (V.KChar c) [] ) | c `elem` ("oz" :: String) -> modify \s -> s { asLastKeys = c : lasts }
       B.VtyEvent (V.EvKey (V.KRight) [V.MShift] ) -> handleWH $ EHistoryEvent HNext
       B.VtyEvent (V.EvKey (V.KLeft) [V.MShift] ) -> handleWH $ EHistoryEvent HPrevious
       B.VtyEvent (V.EvKey (V.KUp) _ ) -> handleWH $ EHistoryEvent HParent
       B.VtyEvent (V.EvKey (V.KDown) _ ) -> handleWH $ EHistoryEvent HChild
       B.VtyEvent (V.EvKey (V.KPageUp) _ ) -> handleWH $ EHistoryEvent HSkipBackward
       B.VtyEvent (V.EvKey (V.KPageDown) _ ) -> handleWH $ EHistoryEvent HSkipForward
       B.VtyEvent (V.EvKey (V.KLeft) [] ) -> handleWH $ EHistoryEvent HPreviousSibling
       B.VtyEvent (V.EvKey (V.KRight) [] ) -> handleWH $ EHistoryEvent HNextSibling
       B.VtyEvent (V.EvKey (V.KChar '=') [] ) -> handleWH $ EHistoryEvent HSetCurrent
       B.VtyEvent (V.EvKey (V.KChar '\\') [] ) -> handleWH $ EHistoryEvent HSwapCurrent
       B.VtyEvent (V.EvKey (V.KEnd) [] ) -> handleWH $ EHistoryEvent HLast
       B.VtyEvent (V.EvKey (V.KChar '|') [] ) -> handleWH $ EHistoryEvent HResetCurrent
       B.VtyEvent (V.EvKey (V.KHome) [] ) -> handleWH $ EHistoryEvent HFirst
       _ -> B.resizeOrQuit ev
 
handleWH ev = 
  do
    -- reset last keys
    modify \s -> s { asLastKeys = [] }
    case ev of 
         ENextMode -> modify nextMode
         EPrevMode -> modify prevMode
         EToggleViewHistory -> modify \s -> s { asDisplayHistory = not (asDisplayHistory s) }
         EToggleDebugShowDiff -> modify \s -> s { asDebugShowDiffs = not (asDebugShowDiffs s) }
         --
         ENextRun -> modify \s -> resetBox $ runUpdated s { asCurrentRun = nextOf (asCurrentRun s) (asShelvesSummary s) }
         ENextBay -> modify \s -> resetBox $ s { asCurrentBay = nextOf (asCurrentBay s) (currentRun s) }
         ENextShelf -> modify \s -> resetBox $ s { asCurrentShelf = nextOf (asCurrentShelf s) (currentBay s) }
         ENextBox -> modify nextBoxThrough
         EPrevRun -> modify \s -> resetBox $ runUpdated s { asCurrentRun = prevOf (asCurrentRun s) (asShelvesSummary s) }
         EPrevBay -> modify \s -> resetBox $ s { asCurrentBay = prevOf (asCurrentBay s) (currentRun s) }
         EPrevShelf -> modify \s -> resetBox $ s { asCurrentShelf = prevOf (asCurrentShelf s) (currentBay s) }
         EPrevBox -> modify prevBoxThrough
         EFirstRun -> modify \s -> resetBox $ runUpdated s { asCurrentRun = 0 }
         ELastRun -> modify \s -> resetBox $ runUpdated s { asCurrentRun = lastOf (asShelvesSummary s) }
         EFirstBay -> modify \s -> resetBox $ s { asCurrentBay = 0 }
         ELastBay -> modify \s -> resetBox $ s { asCurrentBay = lastOf (currentRun s) }
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
         ESetBoxOrder boxOrder -> modify (setBoxOrder boxOrder)
         ENextHLRun -> do
                    asSelected <- gets asSelectedStyle
                    case asSelected of
                         Nothing -> handleWH ESelectCurrentStyle 
                         _ -> return ()
                    modify \s -> case asSelectedStyle s of
                                            Nothing -> s
                                            Just style -> findNextHLRun style s
         EPrevHLRun -> do
                    asSelected <- gets asSelectedStyle
                    case asSelected of
                         Nothing -> handleWH ESelectCurrentStyle 
                         _ -> return ()
                    modify \s -> case asSelectedStyle s of
                                            Nothing -> s
                                            Just style -> findPrevHLRun style s
         ERenderRun -> get >>= liftIO . drawCurrentRun
         EHistoryEvent ev -> navigateHistory ev -- >> modify \s -> s { asDisplayHistory = True }
         EToggleHistoryNavigation -> modify \s -> s { asNavigateCurrent = not (asNavigateCurrent s ) }
         EStartInputSelect mode -> modify \s -> s { asInput = Just (selectInput mode $ makeInputData s) }
         EReload -> error "Should have been caught earlier"
    modify updateHLState
    where resetBox s = s { asCurrentBox = 0 }

setNewWHEvent ev = do
   s@AppState{..} <- get
   new <- liftIO $ execWH asWarehouse do
       let newWH = asWarehouse { whCurrentEvent = ev }
       put newWH
       asShelvesSummary <- makeAppShelvesSummary
       return $ updateHLState s {asWarehouse = newWH, asShelvesSummary  }
   put new
   
navigateHistory HSetCurrent = do
   diff <- gets asDiffEvent 
   setNewWHEvent diff
   modify \s -> s { asDisplayHistory = True }
navigateHistory HSwapCurrent = do
  diff <- gets asDiffEvent
  current <- gets asCurrentEvent
  setNewWHEvent diff
  modify \s -> s { asDiffEvent = current }
navigateHistory ev = do
   s@AppState{..} <- get
   let current = if asNavigateCurrent 
                 then asCurrentEvent s
                 else asDiffEvent
       events = whEventHistory asWarehouse 
       newEventM = case ev of
                     HFirst -> lastMay events
                     HLast -> headMay events
                     HResetCurrent -> Just current
                     HNext -> findNextEvent current events
                     HPrevious -> evPreviousM current
                     HParent -> evParent current
                     HChild -> findFirstChild current events
                     HSkipBackward -> case currentBoxHistory s of
                                        zhistory -> fmap fst $ Map.lookupLT current $ zBefore zhistory
                     HSkipForward -> case currentBoxHistory s of
                                        zhistory -> fmap fst $ Map.lookupGT current $ zBefore zhistory <> zAfter zhistory
                     HNextSibling -> findNextSibling current events
                     HPreviousSibling -> findPreviousSibling current events
   forM_ newEventM \new ->
        if asNavigateCurrent && ev /= HResetCurrent
        then 
          setNewWHEvent new
        else 
          put s {asDiffEvent =  new}
  
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


-- navigate through all boxes changing shelf if needed
nextBoxThrough :: AppState -> AppState
nextBoxThrough s@AppState{..} = let
  next = nextOf asCurrentBox (currentShelf s)
  in if next == asCurrentBox 
     then nextShelfThrough s { asCurrentBox = 0 }
     else s {asCurrentBox = next }
  
nextShelfThrough s@AppState{..} = let
  next = nextOf asCurrentShelf (currentBay s)
  in if next == asCurrentShelf
     then nextBayThrough s { asCurrentShelf = 0 }
     else s { asCurrentShelf = next }
     
nextBayThrough s@AppState{..} = let
  next = nextOf asCurrentBay (currentRun s)
  in if next == asCurrentBay
     then nextRunThrough s { asCurrentBay = 0 }
     else s { asCurrentBay = next }

nextRunThrough s = let
  next = nextOf (asCurrentRun s) (asShelvesSummary s)
  in if next == 0
     then s { asCurrentRun = 0, asCurrentBay = 0 }
     else s { asCurrentRun = next }

prevBoxThrough :: AppState -> AppState
prevBoxThrough s@AppState{..} = let
  prev = prevOf asCurrentBox (currentShelf s)
  in if prev == asCurrentBox 
     then (\s' -> s' { asCurrentBox = lastOf (currentShelf s') }) $ prevShelfThrough s
     else s {asCurrentBox = prev }
  
prevShelfThrough s@AppState{..} = let
  prev = prevOf asCurrentShelf (currentBay s)
  in if prev == asCurrentShelf
     then (\s' -> s' { asCurrentShelf = lastOf (currentBay s')}) $ prevBayThrough s
     else s { asCurrentShelf = prev }
     
prevBayThrough s@AppState{..} = let
  prev = prevOf asCurrentBay (currentRun s)
  in if prev == asCurrentBay
     then (\s' -> s' { asCurrentBay = lastOf (currentRun s') }) $ prevRunThrough s
     else s { asCurrentBay = prev }

prevRunThrough s@AppState{..} = let
  prev = prevOf asCurrentRun asShelvesSummary
  in if prev == 0
     then let lastRunN = lastOf asShelvesSummary
              lastRun = selectFromSumVec lastRunN asShelvesSummary
              lastBayN = lastOf lastRun
              lastBay = selectFromSumVec lastBayN  lastRun
              lastShelfN = lastOf lastBay
              lastShelf = selectFromSumVec lastShelfN  lastBay
          in prevBoxThrough $ s { asCurrentRun = lastRunN
                                 , asCurrentBay = lastBayN
                                 , asCurrentShelf = lastShelfN
                                 , asCurrentBox = lastOf lastShelf
                                 }
     else s { asCurrentRun = prev }

setBoxOrder :: BoxOrder -> AppState -> AppState
setBoxOrder boxOrder state@AppState{..} = AppState{asCurrentRunStyles=sorted,asBoxOrder=boxOrder,..} where
  sorted = case boxOrder of
              BOByName -> sortOn fst asCurrentRunStyles
              BOByVolume -> sortOn (Down . suVolume . snd) asCurrentRunStyles
              BOByCount -> sortOn (Down . suCount . snd) asCurrentRunStyles
              BOByShelve -> let -- find boxes in order of appearance
                  style'shelf :: Map Text (Text, Dimension)
                  style'shelf = Map.fromList $ reverse
                                             [ (boxStyle box , (sName shelfSum, boxOffset box))
                                             | baySum <- sDetailsList (currentRun state)
                                             , shelfSum <- sortOn sName $ sDetailsList baySum
                                             , box <- sortOn boxOffset $ map zCurrentEx $ sDetailsList shelfSum
                                             ]
                  in sortOn (flip lookup style'shelf . fst) asCurrentRunStyles
-- * Find next shelf
-- | Find next shelf containing the given style
-- or highlighted run
findNextHLRun :: Text -> AppState -> AppState
findNextHLRun style AppState{..} = let
   nextRuns = drop (asCurrentRun + 1) (sDetails asShelvesSummary)
   indexM = V.findIndex (isJust . lookup style . sStyles) nextRuns 
   newRun = case indexM of
               Nothing -> asCurrentRun
               Just i -> asCurrentRun + i + 1
   in AppState{asCurrentRun=newRun,..}


findPrevHLRun :: Text -> AppState -> AppState
findPrevHLRun style AppState{..} = let
   nextRuns = take (asCurrentRun) (sDetails asShelvesSummary)
   indexM = V.findIndex (isJust . lookup style . sStyles) $ reverse nextRuns 
   newRun = case indexM of
               Nothing -> asCurrentRun
               Just i -> asCurrentRun - i -1
   in AppState{asCurrentRun=newRun,..}
    
    
drawCurrentRun :: AppState -> IO ()
drawCurrentRun app  = do 
  let wh = asWarehouse app
      run = shelfGroup wh !! asCurrentRun app
  diag <- execWH wh do
              D.renderRun (shelfStyling wh) (boxStyling wh) run
  let filePath = "/tmp/whp-" <> asTitle app <> "-" <> show (asCurrentRun app) <> ".png"
  renderCairo filePath
              (mkSizeSpec2D Nothing (Just 800))
              diag
  void $ rawSystem "xdg-open" [filePath]
  

-- * Post update
-- | update the list of current styles
runUpdated :: AppState -> AppState
runUpdated state@AppState{..} = setBoxOrder asBoxOrder $ AppState{asCurrentRunStyles=styles,..} where
    styles = fromList $ Map.toList $ sStyles (currentRun state)
-- *  Run
runsSideBar :: AppState -> B.Widget Text
runsSideBar state@AppState{..} = B.renderTable $ runsToTable (summaryHLStatus state) (asHistoryRange state) (asViewMode state) asCurrentRun asShelvesSummary 

-- * Styles
stylesSideBar :: AppState -> B.Widget Text
stylesSideBar state@AppState{..} = 
  B.renderTable $ stylesToTable (selectedStyle state) asCurrentStyle $ fmap fst asCurrentRunStyles
-- renderStatus :: AppState -> Widgets
renderStatus state@AppState{..} = let
  mode = B.str (show asSummaryView)
  legend = B.hBox [ B.withAttr (percToAttrName r 0) (B.str [eigthV i]) | i <- [0..8] , let r = fromIntegral i / 8 ]
  in B.vLimit 1 $ B.hBox $ [ B.txt (sName $ currentShelf state)  -- current shelf
                           , B.center $ maybe (B.str "∅") styleNameWithAttr (asSelectedStyle ) -- current style
                           , B.center $ maybe (B.str "∅") (B.txt . sText) (asBoxSelection ) -- current selection
                           , B.center $ B.str "/" B.<+> maybe (B.str "∅") (B.txt . sText) (asShelfSelection ) -- current selection
                           , B.center $ B.str (show asBoxOrder)
                           , B.center mode
                           , surroundIf (not asNavigateCurrent) $ B.txt $ displayEvent asDiffEvent
                           , surroundIf asNavigateCurrent $ B.str $ show (whCurrentEvent asWarehouse)
                           , B.padLeft B.Max legend
                           ]
  where surroundIf False w = w
        surroundIf True  w = B.hBox [B.str "[", w, B.str "]"]
             
debugShelf :: AppState -> B.Widget Text
debugShelf state = let
  ssum = currentShelf state
  in B.vBox $
     if | ViewSummary _ <- asViewMode state 
        ->  [ B.hBox $ intersperse (B.str " ") 
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
        | otherwise 
        -> let boxMap = computeBoxDiffHistoryFrom  $ currentBoxHistory state
           in if asDebugShowDiffs  state
              then [ B.str "Shelf: " B.<+> B.vBox (map (B.str  . show) $ reverse $ mapToList $ seEvents $ sExtra ssum)
                   , B.str "For: " B.<+> B.strWrap (show $ diffFor (asHistoryRange state) $ seEvents $ sExtra ssum)
                   , B.str "BoxH: " B.<+> B.strWrap (show $ currentBoxHistory state)
                   , B.str "BoxDiffs: " B.<+> B.vBox (map (B.str . show) $ (reverse . mapToList $  boxMap))
                   , B.str "For:" B.<+> B.strWrap (show . diffFor (asHistoryRange state) $ boxMap)
                   , B.str "Manual:" B.<+> B.strWrap (let at = zAtWithEvent (asDiffEvent state) (currentBoxHistory state)
                                                      in show  (fmap fst at) <> " " <> " " <> show ( computeBoxDiffM (currentBox state) (fmap snd at)) <> show at
                                                     )
                   , B.hBorder
                   ]
              else []
              ++ [ B.str "WH" B.<=> eventTree (asCurrentEvent state)
                 , B.str "Diff" B.<=> eventTree (asDiffEvent state)
                 ]

  
 -- * Render 
renderRun :: (ZHistory1 Box RealWorld -> B.Widget n) -> Run SumVec (SumVec (ZHistory1 Box RealWorld)) -> [ B.Widget n ]
renderRun renderBox run =  concat 
          [ map (B.padTop B.Max)
            [ B.withAttr (fst bayNameAN )
                             $ B.vBox (map (withHLStatus (seBoxHLStatus $ sExtra bay) . B.str . pure) 
                             $ toList (sName bay <> "▄"))
                             --                     ^^^ aligned with the bottom border of the shelf
            , B.renderTable
            . baySummaryToTable (B.vBox . map renderBox)
            $ bay
            ]
          | bay <- F.toList . sDetails $ run
          ]
          
-- * 
withHLBoxAttr :: AppState -> (Box RealWorld -> B.Widget n) ->  Box RealWorld -> B.Widget n
withHLBoxAttr state f box = withHLStatus (boxHLStatus state box) (f box)


boxHLStatus :: AppState -> Box RealWorld -> HighlightStatus
boxHLStatus state box = HighlightStatus{..} where
    hsCurrent = Just box == currentBox state
    hsHighlighted = if Just (boxStyle box) == selectedStyle state then 1 else 0
    hsSelected = case asBoxSelection state of
                   Just selection | boxId box `elem` sSelected selection -> 1
                   _  -> 0
    
shelfHLStatus :: AppState -> Text -> HighlightStatus
shelfHLStatus state shelf = HighlightStatus{..} where
    hsCurrent = shelf == sName (currentShelf state)
    hsHighlighted = 0
    hsSelected = case asShelfSelection state of
                   Just selection | shelf `elem` sSelected selection -> 1
                   _  -> 0
summaryHLStatus :: AppState -> Run SumVec (SumVec (ZHistory1 Box RealWorld)) -> HighlightStatus
summaryHLStatus state run = foldMap (boxHLStatus state . zCurrentEx) [ box
                                                                     | bay <- sDetailsList run
                                                                     , shelf <- sDetailsList bay
                                                                     , box <- sDetailsList shelf
                                                                     ]

-- * Inputs
setBoxSelection :: Text -> B.EventM n AppState ()
setBoxSelection sel = do
  state  <- get
  asBoxSelection <- liftIO $ execWH (asWarehouse state) $ makeBoxSelection sel
  put $ updateHLState state { asBoxSelection }
  
setShelfSelection :: Text -> B.EventM n AppState ()
setShelfSelection sel = do
  state  <- get
  asShelfSelection <- liftIO $ execWH (asWarehouse state) $ makeShelfSelection sel
  put $ updateHLState state { asShelfSelection }


makeBoxSelection :: Text -> WH (Maybe (Selection (BoxSelector s) (BoxId s))) s
makeBoxSelection "" = return Nothing
makeBoxSelection sText = do 
   let sSelector = parseBoxSelector sText
   boxeIds <- findBoxByNameAndShelfNames sSelector
   let sSelected = Set.fromList $ map boxId boxeIds 
   return $ Just Selection{..}

makeShelfSelection :: Text -> WH (Maybe (Selection (ShelfSelector s) Text)) s
makeShelfSelection "" = return Nothing
makeShelfSelection sText = do 
   let sSelector = parseShelfSelector sText
   shelfIds <- findShelvesByBoxNameAndNames sSelector
   shelves <- mapM findShelf shelfIds
   let sSelected = Set.fromList $ map shelfName shelves
   return $ Just Selection{..}
             
makeInputData :: AppState -> InputData
makeInputData state = let
    idInitial = ""
    idShelf =  sName $ currentShelf state
    idPropertyValue = fromMaybe "" $ selectedStyle state
    idStyle = maybe "" boxStyle $ currentBox state
    idContent = case fmap boxContent $ currentBox state of
                     Just content | not (null content) -> "#'" <> content
                     _ -> ""
    idBoxSelector = maybe "" sText $ asBoxSelection state
    idShelfSelector = maybe "" sText $ asShelfSelection state
    in InputData{..}

                                                                     

                                                                     
