module WarehousePlanner.Brick.App
(
whMain
)
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Summary as S
import WarehousePlanner.History
import WarehousePlanner.Brick.Types
import WarehousePlanner.Brick.Util
import WarehousePlanner.Brick.RenderBar
import WarehousePlanner.Brick.BoxDetail
import WarehousePlanner.Exec (execWH)
import Brick qualified as B
import Brick.Widgets.Border qualified as B
import Graphics.Vty.Attributes qualified as V
import Graphics.Vty.Input.Events qualified as V
import Control.Monad.State (gets, get, modify, put)
import Data.List.NonEmpty(NonEmpty(..), (!!))
import Data.List.NonEmpty qualified as NE
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

type Resource = Text
type WHApp = B.App AppState WHEvent Resource
data WHEvent = ENextMode
             | EPrevMode
             | EToggleViewHistory
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
                  | HSwapCurrent

makeAppShelvesSummary :: WH (Runs SumVec (SumVec  (History Box RealWorld))) RealWorld
makeAppShelvesSummary = do
  runs <- gets shelfGroup
  shelvesSummary <- traverseRuns findShelf runs
                 >>= makeRunsSummary (makeExtra)
                 >>= traverseRuns findBoxes
  let toL :: forall a . ShelvesSummary SummaryExtra NonEmpty a -> SumVec a
      toL ssum@ShelvesSummary{..} = let
          sumZip = fromList $ sDetailsList ssum
          in ShelvesSummary{sDetails=sumZip,..}
  return $ fromRuns toL toL toL
                       $ mapRuns (\s -> s { sDetails = fromList $ sortOn boxOrder  $ sDetails s }
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
                        let boxEventMaps = [ computeBoxDiffHistoryFrom currentEvent history
                                          |  history <- toList boxHistorys
                                          ]
                            shelfEventMap = computeShelfDiffHistoryFrom currentEvent shelfHistory
                            eventMap = unionsWith (<>) $ shelfEventMap : boxEventMaps
                        eventsWithName <- flip traverse eventMap \(DiffStatus{..}) -> do
                                               namesIn <-  mapM findShelf $ toList dsBoxIn
                                               namesOut <-  mapM findShelf $ toList dsBoxOut
                                               return (DiffStatus { dsBoxIn=setFromList $ map shelfName namesIn
                                                                  , dsBoxOut=setFromList $ map shelfName namesOut
                                                                  , ..})
                        return $ SummaryExtra styleMap (eventsWithName)

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
                 , asDiffEventStack = []
                 , ..}


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
                                              : [ B.vBox $ (map B.hBox)
                                                         [ renderRun (\bhistory' ->
                                                                         let box = fromHistory bhistory
                                                                             bhistory = case NE.dropWhile (\(e,_) -> e > whCurrentEvent asWarehouse ) bhistory' of
                                                                                         [] -> bhistory' -- TODO HACK
                                                                                         (x:xs) -> x :| xs
                                                                             -- ^ bHistory' has ALL the box event, including after current event
                                                                             rendered =  renderBoxOrientation (currentBox s) box
                                                                             currentShelf = "<currentShelf>"
                                                                             mkCurrent set = if null set then mempty else singletonSet currentShelf
                                                                         in if asDisplayHistory
                                                                            then historyIndicator rendered currentShelf (asHistoryRange s) (mapFromList [ (e, d { dsBoxIn = mkCurrent (dsBoxIn d), dsBoxOut = mempty })
                                                                                                                                                                  | (e, d) <- mapToList $ computeBoxDiffHistoryFrom (whCurrentEvent $ asWarehouse) bhistory
                                                                                                                                                                  ]
                                                                                                                                                     )
                                                                            else rendered
                                                                     )
                                                                     (currentRun s)
                                                         , [B.hBorder]
                                                         , renderRun (renderBoxContent (currentBox s) . fromHistory) (let run = currentRun s
                                                                                                        in run { sDetails = drop asCurrentBay $ sDetails run }
                                                                                                        )
                                                         ]
                                              ]
                  mainRun = B.emptyWidget -- renderHorizontalRun asSummaryView (currentRun s)
              in  [ vBoxB [ mainRun
                           , B.vLimit (if asDisplayHistory then 21 else 11) $ hBoxB (debugShelf s :  maybe [] (pure . boxDetail) (currentBox s))
                           , main
                           , renderStatus s
                           ]
                  ]
      appChooseCursor = B.neverShowCursor
      appHandleEvent = whHandleEvent
      appAttrMap state = B.attrMap V.defAttr $ generateLevelAttrs  <> extraAttrs state
      appStartEvent = return ()
  in app
  
whMain :: String -> Warehouse RealWorld -> IO ()
whMain title wh = do
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
      attrs state =
            selectedAttr
            -- : bayNameAN
            : boldAttr : tagNameAttr : virtualTagAttr : specialTagAttr
            : zipWith (\style attr -> (makeStyleAttrName False style, reverseIf (Just style == selectedStyle state) attr ))
                      styles
                      (cycle defaultStyleAttrs)
            <> eventAttrs
            <> zipWith (\style attr -> (makeStyleAttrName True style, reverseIf (Just style == selectedStyle state) $ V.withBackColor (V.withStyle attr V.bold)
                                                                                                                    $ if (Just style == selectedStyle state)
                                                                                                                      then V.white 
                                                                                                                      else V.color240 50 50 50 ))
                      styles
                      (cycle defaultStyleAttrs)
  void $ B.defaultMain (whApp attrs) state0

reverseIf :: Bool -> V.Attr -> V.Attr
reverseIf True attr = attr `V.withStyle` V.reverseVideo
reverseIf _ attr = attr


whHandleEvent :: B.BrickEvent Resource WHEvent -> B.EventM Resource AppState ()
whHandleEvent ev = do
  lasts <- gets asLastKeys
  case ev of 
       B.AppEvent e -> handleWH e
       B.VtyEvent (V.EvKey (V.KChar 'n') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByName
       B.VtyEvent (V.EvKey (V.KChar 's') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByShelve
       B.VtyEvent (V.EvKey (V.KChar 'c') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByCount
       B.VtyEvent (V.EvKey (V.KChar 'v') [] ) | 'o':_ <- lasts  -> handleWH $ ESetBoxOrder BOByVolume
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
       B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
       B.VtyEvent (V.EvKey (V.KChar 'q') [] ) -> B.halt
       B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl] ) -> B.halt
       B.VtyEvent (V.EvKey (V.KChar ']') [] ) -> handleWH ENextHLRun
       B.VtyEvent (V.EvKey (V.KChar '[') [] ) -> handleWH EPrevHLRun
       B.VtyEvent (V.EvKey (V.KChar 'v') [] ) -> handleWH ERenderRun
       B.VtyEvent (V.EvKey (V.KChar c) [] ) | c `elem` ("o" :: String) -> modify \s -> s { asLastKeys = c : lasts }
       B.VtyEvent (V.EvKey (V.KRight) [] ) -> handleWH $ EHistoryEvent HNext
       B.VtyEvent (V.EvKey (V.KLeft) [] ) -> handleWH $ EHistoryEvent HPrevious
       B.VtyEvent (V.EvKey (V.KUp) [] ) -> handleWH $ EHistoryEvent HParent
       B.VtyEvent (V.EvKey (V.KDown) [] ) -> handleWH $ EHistoryEvent HChild
       B.VtyEvent (V.EvKey (V.KLeft) [V.MShift] ) -> handleWH $ EHistoryEvent HSkipBackward
       B.VtyEvent (V.EvKey (V.KRight) [V.MShift] ) -> handleWH $ EHistoryEvent HSkipForward
       B.VtyEvent (V.EvKey (V.KLeft) [V.MCtrl] ) -> handleWH $ EHistoryEvent HPreviousSibling
       B.VtyEvent (V.EvKey (V.KRight) [V.MCtrl] ) -> handleWH $ EHistoryEvent HNextSibling
       B.VtyEvent (V.EvKey (V.KChar '=') [] ) -> handleWH $ EHistoryEvent HSetCurrent
       B.VtyEvent (V.EvKey (V.KChar '\\') [] ) -> handleWH $ EHistoryEvent HSwapCurrent
       B.VtyEvent (V.EvKey (V.KEnd) [] ) -> handleWH $ EHistoryEvent HResetCurrent
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
         EHistoryEvent ev -> navigateHistory ev
    where resetBox s = s { asCurrentBox = 0 }

navigateHistory HSetCurrent = do
   s@AppState{..} <- get
   new <- liftIO $ execWH asWarehouse do
       let newWH = asWarehouse { whCurrentEvent = asDiffEvent }
       put newWH
       asShelvesSummary <- makeAppShelvesSummary
       return s {asWarehouse = newWH,asShelvesSummary, asDisplayHistory = True }
   put new
navigateHistory HSwapCurrent = do
  AppState{..} <- get
  let current = whCurrentEvent (asWarehouse)
  modify \s -> s {asWarehouse = asWarehouse { whCurrentEvent = asDiffEvent } }
  navigateHistory HSetCurrent
  modify \s -> s {asDiffEvent  = current }

navigateHistory ev = modify \s@AppState{..} -> 
  if asDiffEvent == NoHistory
  then s
  else let
       pushCurrent = asDiffEvent : asDiffEventStack
       new = case ev of 
              HPrevious -> case evPrevious asDiffEvent of
                             NoHistory -> Nothing
                             prev -> Just (prev, pushCurrent)
              HNext -> case asDiffEventStack of
                        [] -> Nothing
                        (e:stack) -> Just (e, stack)
              HParent -> case evParent asDiffEvent of
                           Nothing -> Nothing
                           Just p -> Just (p, pushCurrent)
              _ -> Nothing
       in case new of
          Nothing -> s
          Just (new, stack) -> s { asDiffEvent = new, asDiffEventStack = stack, asDisplayHistory = True }

  
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
                                             , box <- sortOn boxOffset $ map fromHistory $ sDetailsList shelfSum
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
runsSideBar state@AppState{..} = B.renderTable $ runsToTable (asHistoryRange state) (selectedStyle state) (asViewMode state) asCurrentRun asShelvesSummary 

-- * Styles
stylesSideBar :: AppState -> B.Widget Text
stylesSideBar state@AppState{..} = 
  B.renderTable $ stylesToTable (selectedStyle state) asCurrentStyle $ fmap fst asCurrentRunStyles
-- renderStatus :: AppState -> Widgets
renderStatus state@AppState{..} = let
  mode = B.str (show asSummaryView)
  legend = B.hBox [ B.withAttr (percToAttrName r 0) (B.str [eigthV i]) | i <- [0..8] , let r = fromIntegral i / 8 ]
  in B.vLimit 1 $ B.hBox $ [ B.txt (sName $ currentShelf state)  -- current shelf
                           , B.center $ maybe (B.str "∅") (styleNameWithAttr False) (asSelectedStyle ) -- current style
                           , B.center $ B.str (show asBoxOrder)
                           , B.center mode
                           , B.txt $ displayEvent asDiffEvent
                           , B.str $ show (whCurrentEvent asWarehouse)
                           , B.padLeft B.Max legend
                           ]
             
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
        -> let boxMap = computeBoxDiffHistoryFrom (whCurrentEvent $ asWarehouse state) <$> currentBoxHistory state
           in [ B.str "Shelf: " B.<+> B.vBox (map (B.str  . show) $ reverse $ mapToList $ seEvents $ sExtra ssum)
           , B.str "For: " B.<+> B.strWrap (show $ diffFor (asHistoryRange state) $ seEvents $ sExtra ssum)
           , B.str "BoxH: " B.<+> B.strWrap (maybe "" (show ) $ currentBoxHistory state)
           , B.str "BoxDiffs: " B.<+> B.vBox (map (B.str . show) $ maybe [] (reverse . mapToList) boxMap)
           , B.str "For:" B.<+> B.strWrap (maybe "" (show . diffFor (asHistoryRange state)) boxMap)
           ]

  
 -- * Render 
renderRun :: (History Box RealWorld -> B.Widget n) -> Run SumVec (SumVec (History Box RealWorld)) -> [ B.Widget n ]
renderRun renderBox run =  concat 
          [ map (B.padTop B.Max)
            [ B.withAttr (fst bayNameAN )
                             $ B.vBox (map (B.str . pure) 
                             $ toList (sName bay <> "▄"))
                             --                     ^^^ aligned with the bottom border of the shelf
            , B.renderTable
            . baySummaryToTable (B.vBox . map renderBox)
            $ bay
            ]
          | bay <- F.toList . sDetails $ run
          ]
