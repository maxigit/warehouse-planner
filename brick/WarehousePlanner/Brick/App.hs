{-# LANGUAGE UndecidableInstances #-}
module WarehousePlanner.Brick.App
(
whMain
)
where

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Move
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
import Brick.Keybindings qualified as B
import Data.List (cycle)
import Data.Map qualified as Map
import WarehousePlanner.Brick.Table
import Data.Vector qualified as V
import WarehousePlanner.Display qualified as D
import Diagrams.Backend.Cairo (renderCairo)
import Diagrams (mkSizeSpec2D)
import System.Process (rawSystem)
import Data.Set qualified as Set
import Text.Printf
-- import System.Clipboard(setClipboardString)

type WHApp = B.App AppState WHEvent Resource
data SearchDirection = Forward |  Backward
    deriving (Eq, Show, Ord, Enum, Bounded)
data WHEvent = ENextMode
             | EPrevMode
             | EToggleViewHistory
             | EToggleDebugShowDiff
             | EToggleCollapseDepth
             | ETogglePropertyGradient
             | EToggleShowSelected
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
             | EFindNextBox SearchDirection
             -- 
             | ESelectCurrentPropValue
             | ENextPropValue
             | EPreviousPropValue
             -- 
             | ESetBoxOrder BoxOrder
             | ESetProperty Text
             | ESetBoxTitle Text
             -- 
             | ENextHLRun
             | EPrevHLRun
             -- 
             | ERenderRun
             --
             | EHistoryEvent HistoryEvent
             | EToggleHistoryNavigation
             | EToggleHistoryPrevious
             --
             -- Input
             | EStartInputSelect InputMode
             -- Submap
             | ESubMap Char
             | EDisplayMainHelp
             -- Misc
             | EReload
             | EQuit
             -- Action
             | EMove
             -- Yanks
             | EYankBoxDetails Bool -- ^ editor or yank

     deriving (Show, Eq, Ord)
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
     deriving(Show, Eq, Ord, Enum, Bounded)

makeAppShelvesSummary :: Maybe Text -> WH (Runs SumVec (SumVec  (ZHistory1 Box RealWorld))) RealWorld
makeAppShelvesSummary propm = do
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
                        let getProp = case propm of
                                           Just prop -> case expandAttributeMaybe prop of
                                                             Nothing -> \_ _ -> return ""
                                                             Just expand -> expand
                                           Nothing -> \b _ -> return $ boxStyle b
                        propValues <- zipWithM (\b i -> do
                                                        p' <- getProp b i
                                                        let p = if p' == ""
                                                                then "∅"
                                                                else p'
                                                        -- Hack to store the property value for display
                                                        updateBoxTags [("@prop", SetValues [p])] b 0
                                                        return (p , makeBoxesSummary [b])
                                               )
                                               (toList boxes)
                                               [1..]

                        let propValueMap = Map.fromListWith (<>) propValues
                        boxesWithProp <- mapM findBox (_shelfBoxes shelf)
                        boxHistorys <- mapM getBoxHistory boxesWithProp
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
                        return $ SummaryExtra propValueMap (eventsWithName) mempty mempty

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

initState :: (AppState -> AppState) -> String -> WH (AppState) RealWorld
initState adjust title = do
  let asSummaryView = SVVolume
      asDisplayHistory = False
      state = adjust $ AppState
                  { asCurrentRun=0, asCurrentBay = 0, asCurrentShelf = 0, asCurrentBox = 0
                  , asProperty = Nothing, asSelectedPropValue = Nothing, asCurrentPropValue = 0, asCurrentRunPropValues = mempty
                  , asPropertyAsGradient = Nothing
                  , asShowSelected = True
                  , asBoxOrder = BOByShelve
                  , asSubmap = Nothing, asDisplayMainHelp = False
                  , asWarehouse = error "Warehouse not initialized"
                  , asTitle = title
                  , asDiffEvent = NoHistory
                  , asNavigateCurrent = False
                  , asNavigateWithPrevious = True
                  , asDebugShowDiffs = False
                  , asInput = Nothing, asInputHistory = mempty
                  , asBoxSelection = Nothing
                  , asShelfSelection = Nothing
                  , asShelvesSummary = error "Shelves Summary not initialized"
                  , asCollapseDepth = True
                  , ..
                  }
  asShelvesSummary <- makeAppShelvesSummary (asProperty state)
  warehouse <- get
  return . runUpdated $ state { asWarehouse = warehouse
                              , asDiffEvent = whCurrentEvent warehouse
                              ,asShelvesSummary
                              }


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
                                                         [ renderRun project
                                                                     (\bhistory ->
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
                                                         , renderRun project
                                                                     (withHLBoxAttr s renderBoxContent .  zCurrentEx)
                                                                     (let run = currentRun s
                                                                      in run { sDetails = drop asCurrentBay $ sDetails run }
                                                                     )
                                                         ]
                                              ]
                  project d = if asCollapseDepth then d {dWidth = 0 } else d
                  mainRun = B.emptyWidget -- renderHorizontalRun asSummaryView (currentRun s)
                  help = case asSubmap of 
                           Nothing | asDisplayMainHelp == False  -> B.emptyWidget
                           _ -> B.centerLayer $ submapHelp asSubmap
              in  [ help
                  , vBoxB [ mainRun
                           , let chunk = if asDisplayHistory 
                                         then 40
                                         else 13
                             in B.vLimit chunk $ hBoxB (debugShelf s :  (pure . boxDetail (\_ _ -> chunk) asWarehouse (asHistoryRange s)) (currentBoxHistory s))
                           , main
                           , maybe (renderStatus s) renderInput asInput
                           ]
                  ]
      appChooseCursor = B.neverShowCursor
      appHandleEvent = whHandleEvent reload
      appAttrMap state = B.attrMap V.defAttr $ generateLevelAttrs  <> extraAttrs state
      appStartEvent = return ()
  in app
  
submapHelp :: Maybe Char -> B.Widget Text
submapHelp cm = let 
   Just section'handlers = lookup cm handlerMap
   Just conf = lookup cm keyConfigMap
   in B.border $ B.txt $ B.keybindingTextTable conf section'handlers
   -- in B.vBox [ B.borderWithLabel (B.txt section) $ B.keybindingHelpWidget conf handlers 
   --           | (section, handlers) <- section'handlers
   --           ]
  
whMain :: (AppState -> AppState) -> String -> (IO (Either Text (Warehouse RealWorld))) -> IO ()
whMain adjust title reload = do
  -- error $ unpack
  --       $ unlines 
  --       $ [ B.keybindingTextTable keyConfig handlers
  --         | ((__section'cm, handlers), keyConfig) <- zip handlerGroups (map snd keyConfigGroups)
  --         --                             ^
  --         --                             +---- same as sectionm
  --         ]
  whE <- reload
  let wh = case whE of
            Left e -> error (unpack e)
            Right w -> w
  state0 <- execWH wh $ initState adjust title 
  -- to avoid styles to have the same colors in the same shelf
  -- we sort them by order of first shelves
  let attrs state =
            selectedAttr
            -- : bayNameAN
            : boldAttr : tagNameAttr : virtualTagAttr : specialTagAttr
            : propAttrs state
            <> eventAttrs
            <> highlightAttrs
      propAttrs state = 
            case asPropertyAsGradient state of
               Nothing -> zipWith (\style attr -> (makeStyleAttrName style, attr))
                                  (keys $ sePropValues $ sExtra $ asShelvesSummary  state)
                                  (cycle defaultStyleAttrs)
               Just allProps -> let props = if allProps
                                            then keys $ sePropValues $ sExtra $ asShelvesSummary  state
                                            else map fst $ toList $ asCurrentRunPropValues state
                                in gradientAttributes props
  void $ B.defaultMain (whApp attrs reload) state0



keyBindingGroups :: [(Maybe (Text, Char),[ (Text,  [([B.Binding], WHEvent, Text, _)])])]
keyBindingGroups =  groups
  where  groups = [ (Nothing , [("Main",                       [ mk 'm' ENextMode "next summary view mode"
                                                               , mk 'M' EPrevMode "previous summary view mode"
                                                               , mk 'v' ERenderRun "visualize current run (jpg)"
                                                               , mK "f1 C-h" EDisplayMainHelp "display main keybindings"
                                                               , mK "q C-c" (EQuit) "Quit"
                                                               , mK "C-r" (EReload) "Reload"
                                                               ])
                               ,("Navigation",                 [ mk 'g' (EFirstRun) "first run"
                                                               , mk 'G' (ELastRun) "last run"
                                                               , mk 'j' (ENextRun) "next run"
                                                               , mk 'k' (EPrevRun) "previous run"
                                                               , mk 'l' (ENextBay) "next bay"
                                                               , mk 'h' (EPrevBay) "previous bay"
                                                               , mk '^' (EFirstBay) "first bay"
                                                               , mk '$' (ELastBay) "last bay"
                                                               , mk 'J' (ENextShelf) "next shelf"
                                                               , mk 'K' (EPrevShelf) "previous shelf"
                                                               , mk 'b' (ENextBox) "next box"
                                                               , mk 'B' (EPrevBox) "previous box"
                                                               ])
                               ,("Search & Highlight",         [ mk '/' (EStartInputSelect ISelectBoxes) "search boxes by pattern"
                                                               , mk '?' (EStartInputSelect ISelectShelves) "search shelves by box pattern "
                                                               , mk ']' ENextHLRun "next highlight run"
                                                               , mk '[' EPrevHLRun "previous highlight run"
                                                               , mk 'n' (EFindNextBox Forward) "next searched/highlighted box"
                                                               , mk 'N' (EFindNextBox Backward) "previous searched/highlighted box"
                                                               , mk V.KEnter ESelectCurrentPropValue "pin current property value"
                                                               , mk '>' (ENextPropValue) "next property value"
                                                               , mk '<' (EPreviousPropValue) "previous property value"
                                                               ])
                               ,("Warehouss operations",       [ mk '%' EMove "Move selected boxes to selected shelves"
                                                               ])
                               ,("History",                    [ mK "S-right" (EHistoryEvent HNext) "next event"
                                                               , mK "S-left" (EHistoryEvent HPrevious) "previous event"
                                                               , mk V.KUp (EHistoryEvent HParent) "go to parent event"
                                                               , mk V.KDown (EHistoryEvent HChild) "go to child" 
                                                               , mk V.KPageUp (EHistoryEvent HSkipBackward) "next current box event"
                                                               , mk V.KPageDown (EHistoryEvent HSkipForward) "previous curren box event"
                                                               , mk V.KLeft (EHistoryEvent HPreviousSibling) "previous sibling"
                                                               , mk V.KRight (EHistoryEvent HNextSibling) "next sibling"
                                                               , mk '=' (EHistoryEvent HSetCurrent) "set current to 'previous'"
                                                               , mk '\\' (EHistoryEvent HSwapCurrent) "swap current and previous position"
                                                               , mk V.KEnd (EHistoryEvent HLast) "end of history"
                                                               , mk '|' (EHistoryEvent HResetCurrent) "reset both 'current' and 'previous' event to end of history"
                                                               , mk V.KHome (EHistoryEvent HFirst) "beginning of history"
                                                               , mk '\t'(EToggleViewHistory) "view/hide history"
                                                               ])
                               ] <> [("Submaps", submaps)]
                  ),(Just ("Order", 'o') , [("Property order", [ mk 'n' (ESetBoxOrder BOByName) "sort by box name"
                                                               , mk 's' (ESetBoxOrder BOByShelve) "sort by position in shelf"
                                                               , mk 'c' (ESetBoxOrder BOByCount) "sort by number of boxes"
                                                               , mk 'v' (ESetBoxOrder BOByVolume) "sort by total volumes"
                                                               ])
                                           ]
                  ),(Just ("Toggle", 'z'), [("History",        [ mk 'h' (EToggleHistoryNavigation) "navigation current/previous"
                                                               , mk 'H' (EToggleHistoryPrevious) "move current and previous"
                                                               ])
                                           ,("Misc",           [ mk 'd' (EToggleDebugShowDiff) "show/hide diff debug info for current shelf"
                                                               , mk 'w' (EToggleCollapseDepth) "Display shelf depth as "
                                                               , mk 'p' (ETogglePropertyGradient) "color properties: random/gradient/gradient (full)"
                                                               , mk 's' (EToggleShowSelected)     "highlight selected property"
                                                               ])
                                           ]
                  ),(Just ("Property", 'p'), [("Preset",       [ mk 'p' (EStartInputSelect ISelectProperty) "manual"
                                                               , mk 'b' (ESetProperty "${boxname}") "boxname"
                                                               , mk 'B' (ESetProperty "$[batch]")   "batch"
                                                               , mk 'c' (ESetProperty "${content}") "content"
                                                               , mk 'C' (ESetProperty "${con}")     "short contnet"
                                                               , mk 'd' (ESetProperty "${dimension}") "box dimension"
                                                               , mk 'D' (ESetProperty "${style:-}-${dimension}") "short style & dimension"
                                                               , mk ':' (ESetProperty "${con}:$[batch]") "short content & batch"
                                                               , mk 't' (ESetProperty "$[ctitle]") "current title"
                                                               , mk 's' (ESetProperty "${style}") "style"
                                                               , mk 'S' (ESetProperty "${style:-}-$[batch]") "short style & batch"
                                                               , mk 'o' (ESetProperty "${orientation}") "orientation"
                                                               , mk 'v' (ESetProperty "${volume}") "volume"
                                                               , mk 'O' (ESetProperty "$[@check]$[@overlap? O]") "Stickout check"
                                                               , mK "C-o" (ESetProperty "$[@ovolume]") "overlapping volume"
                                                               , mk 'g' (ESetProperty "$[@ogroup]") "overlapping group"
                                                               ])
                                             ]
                  ),(Just ("Title", 't'), [("Preset",          [ mk 't' (EStartInputSelect ISelectTag) "manual"
                                                               , mk 'S' (ESetBoxTitle "${style}") "style"
                                                               , mk 's' (ESetBoxTitle "${style:-}") "short style"
                                                               , mk 'c' (ESetBoxTitle "${content}") "content"
                                                               , mk 'b' (ESetBoxTitle "${boxname}") "boxname"
                                                               , mk 'b' (ESetBoxTitle "$[batch]") "batch"
                                                               , mk ':' (ESetBoxTitle "${con}:$[batch]") "short content & batch"
                                                               , mk 'C' (ESetBoxTitle "${con}") "short content"
                                                               , mk 'v' (ESetBoxTitle "${volume}") "volume"
                                                               , mk 't' (ESetBoxTitle "") "current property"
                                                               ])
                                           ]
                  ),(Just ("Yank/Edit", 'y'), [("Yank",        [ mk 'b' (EYankBoxDetails False) "yank box details to clipboard" 
                                                               , mk 'B' (EYankBoxDetails True) "edit box details" 
                                                               ])
                                           ]
                  )
                  ]
         mk binding event desc = ( [B.bind binding], event, desc, handleWH event)
         mK t event desc = case traverse B.parseBinding (words t) of
                                     Left err -> error err
                                     Right bindings -> ( bindings, event, desc, handleWH event)
         submaps = [  mk key (ESubMap key) (sub <> " submap")
                   | (Just (sub, key), _ ) <- groups
                   ]
flattenSections :: [(Text, [a])] -> [a]
flattenSections = concatMap snd
keyEventGroups :: [(Maybe (Text, Char), B.KeyEvents WHEvent)]
keyEventGroups = map (fmap \keyBindings -> B.keyEvents [ (tshow event, event)  | (_, event, _, _) <- flattenSections keyBindings])
                     keyBindingGroups
defaultBindingGroups = map (fmap \keyBindings -> [(event, k) | (k, event, _, _) <- flattenSections keyBindings ])
                           keyBindingGroups
keyConfigGroups = map (fmap \(defaultBindings, keyEvents) -> B.newKeyConfig keyEvents defaultBindings [])
                      $ zipGroup defaultBindingGroups keyEventGroups
keyDispatcherGroups = map (fmap \(handlers, keyConfig) ->
                                case B.keyDispatcher keyConfig (flattenSections handlers) of 
                                     Left errors -> error $ show $ [ (k, map (B.handlerDescription . B.kehHandler . B.khHandler) handlers)
                                                                 | (k, handlers) <- errors
                                                                 ]
                                     Right d -> d)
                          $ zipGroup handlerGroups keyConfigGroups
keyDispatcherMap = groupsToMap keyDispatcherGroups
keyConfigMap = groupsToMap keyConfigGroups
handlerMap = groupsToMap handlerGroups
handlerGroups :: [(Maybe (Text, Char), [ (Text, [ B.KeyEventHandler WHEvent _ ] )])]
handlerGroups = map (fmap (map $ fmap (\keyBindings -> [ B.onEvent ev desc action | (_, ev, desc, action) <- keyBindings ])
                          )
                    )
                    keyBindingGroups
groupsToMap :: [ (Maybe (Text, Char), a) ] -> Map (Maybe Char) a
groupsToMap groups = Map.fromList [(fmap snd section'cm, group) | (section'cm, group) <- groups ]


zipGroup :: [(a,b)] -> [(a, c)] -> [(a, (b,c))]
zipGroup = zipWith melt where melt (a, b) (_, c) = (a, (b, c))

whHandleEvent :: (IO (Either Text (Warehouse RealWorld))) -> B.BrickEvent Resource WHEvent -> B.EventM Resource AppState ()
whHandleEvent reload ev = do
  lasts <- toList <$> gets asSubmap
  inputM <- gets asInput
  case ev of 
       _ | Just input <- inputM -> do
             result <- B.nestEventM input $ handleInputEvent ev
             case snd result of
                Left (Just result) -> do
                               case iMode input of
                                    ISelectBoxes -> setBoxSelection result
                                    ISelectShelves -> setShelfSelection result
                                    ISelectProperty -> setProperty result
                                    ISelectTag -> setTagAll result
                               modify \s -> s  { asInput = Nothing 
                                               , asInputHistory = Map.alter (pure . maybe [result] (result:)) 
                                                                            (iMode input)
                                                                            (asInputHistory s)
                                               }
                Left Nothing -> -- ignore
                         modify \s -> s { asInput = Nothing }
                Right input -> modify \s -> s { asInput = Just input }
       B.AppEvent EReload -> do
                  newWHE <- liftIO reload
                  case newWHE of
                       Left e -> error $ unpack e
                       Right newWH -> do
                             modify \s -> s { asWarehouse = newWH }
                             setNewWHEvent $ whCurrentEvent newWH
       B.AppEvent e -> handleWH e
       B.VtyEvent (V.EvKey key mods)  -> do
                  let Just keyDispatcher = lookup (headMay lasts) keyDispatcherMap
                  handled <- B.handleKey keyDispatcher key mods
                  when (not handled) do
                       modify \s -> s { asSubmap = Nothing , asDisplayMainHelp = False }
       _ -> B.resizeOrQuit ev
 
handleWH ev = 
  do
    -- reset last keys
    modify \s -> s { asSubmap = Nothing
                   , asDisplayMainHelp = False
                   }
    case ev of 
         ENextMode -> modify nextMode
         EPrevMode -> modify prevMode
         EToggleViewHistory -> modify \s -> s { asDisplayHistory = not (asDisplayHistory s) }
         EToggleDebugShowDiff -> modify \s -> s { asDebugShowDiffs = not (asDebugShowDiffs s) }
         EToggleCollapseDepth -> modify \s -> s { asCollapseDepth = not (asCollapseDepth s) }
         ETogglePropertyGradient -> modify \s -> s { asPropertyAsGradient = nextPGradient (asPropertyAsGradient s ) }
         EToggleShowSelected -> modify \s -> s { asShowSelected = not  (asShowSelected s) }
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
         EFindNextBox direction -> modify \s -> let conditionM = asum [ asBoxSelection s >>=  \sel -> case sSelected sel of
                                                                                              selected | null selected -> Nothing 
                                                                                              selected -> Just \box -> boxId box `member` selected
                                                                      , flip fmap (selectedPropValue s) \_ -> \box -> hsHighlighted (boxHLStatus s box ) > 0
                                                                      ]
                                      in case fmap (flip (findNextBox direction) s) conditionM of
                                           Just (new, Just _) -> new
                                           _ -> s
         ESelectCurrentPropValue -> modify \s -> s { asSelectedPropValue = if currentPropValue s == asSelectedPropValue s
                                                                   then Nothing
                                                                   else currentPropValue s
                                               }
         ENextPropValue -> do
                    modify \s -> s { asCurrentPropValue = nextOf' (asCurrentPropValue s) (asCurrentRunPropValues s) 
                                   , asShowSelected = True
                                   }
                    -- handleWH ESelectCurrentPropValue
         EPreviousPropValue -> do
                    modify \s -> s { asCurrentPropValue = prevOf' (asCurrentPropValue s) (asCurrentRunPropValues s)
                                   , asShowSelected = True
                                   }
                    -- handleWH ESelectCurrentPropValue
         ESetBoxOrder boxOrder -> modify (setBoxOrder boxOrder)
         ESetProperty prop -> setProperty prop
         ESetBoxTitle "" -> do 
                               propm <- gets asProperty
                               case propm of 
                                    Nothing -> return ()
                                    Just prop -> setBoxTitle prop
         ESetBoxTitle title -> setBoxTitle title
         ENextHLRun -> do
                    asSelected <- gets asSelectedPropValue
                    case asSelected of
                         Nothing -> handleWH ESelectCurrentPropValue 
                         _ -> return ()
                    modify \s -> case asSelectedPropValue s of
                                            Nothing -> s
                                            Just style -> findNextHLRun style s
         EPrevHLRun -> do
                    asSelected <- gets asSelectedPropValue
                    case asSelected of
                         Nothing -> handleWH ESelectCurrentPropValue 
                         _ -> return ()
                    modify \s -> case asSelectedPropValue s of
                                            Nothing -> s
                                            Just style -> findPrevHLRun style s
         ERenderRun -> get >>= liftIO . drawCurrentRun
         EHistoryEvent ev -> navigateHistory ev -- >> modify \s -> s { asDisplayHistory = True }
         EToggleHistoryNavigation -> modify \s -> s { asNavigateCurrent = not (asNavigateCurrent s ) }
         EToggleHistoryPrevious -> modify \s -> s { asNavigateWithPrevious = not (asNavigateWithPrevious s ) }
         EStartInputSelect mode -> modify \s -> s { asInput = Just (selectInput mode $ makeInputData mode s) }
         EReload -> error "Should have been caught earlier"
         ESubMap c -> modify \s -> s { asSubmap = Just c } 
         EDisplayMainHelp -> modify \s -> s { asDisplayMainHelp = True }
         EQuit -> B.halt
         EMove -> do
                  state <- get
                  let currentEvent = whCurrentEvent (asWarehouse state)
                  resultm <- liftIO $ execWH (asWarehouse state) do
                      case (asBoxSelection state , asShelfSelection state) of
                          (Just bs, Just ss) ->  do
                             shelves <- findShelvesByBoxNameAndNames (sSelector ss)
                             newBaseEvent "MOVE %" $ sText bs <> " TO " <> sText ss 
                             leftOver <- excludedList <$> moveBoxes ExitLeft PBestEffort SortBoxes (toList $ sSelected bs) shelves
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
         EYankBoxDetails useEditor -> do
                         s@AppState{..} <- get
                         let text = boxDetailsTextTable  asWarehouse (asHistoryRange s) (currentBoxHistory s)
                             extm = if useEditor then (Just ".tsv") else Nothing
                         liftIO $ yankOrEdit extm text
    modify updateHLState
    where resetBox s = s { asCurrentBox = 0 }

setNewWHEvent ev = do
   s@AppState{..} <- get
   new <- liftIO $ execWH asWarehouse do
       let newWH = asWarehouse { whCurrentEvent = ev }
       put newWH
       asShelvesSummary <- makeAppShelvesSummary asProperty
       return $ updateHLState s {asWarehouse = newWH, asShelvesSummary  }
   put new
   
execute :: WH a RealWorld -> B.EventM  n AppState a
execute action = do 
   s@AppState{..} <- get
   (newState, r) <- liftIO $ execWH asWarehouse do
      result <- action 
      asShelvesSummary <- makeAppShelvesSummary asProperty
      newWH <- get
      return $ (updateHLState s {asWarehouse = newWH, asShelvesSummary  }, result)
   put $ runUpdated newState
   return  r

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
       setDiff e = put s { asDiffEvent = e }
   forM_ newEventM \new -> do
        if asNavigateCurrent && ev /= HResetCurrent
        then 
          setNewWHEvent new
        else 
          setDiff new
        when asNavigateWithPrevious
           if asNavigateCurrent
           then forM_ (evPreviousM new) setDiff 
           else forM_ (findNextEvent new events) setNewWHEvent
             


  
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


nextPGradient :: Maybe Bool -> Maybe Bool
nextPGradient = \case 
              Nothing -> Just True
              Just True -> Just False
              Just False -> Nothing

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
  in if next == (asCurrentRun s)
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
setBoxOrder boxOrder state@AppState{..} = AppState{asCurrentRunPropValues=sorted,asBoxOrder=boxOrder,..} where
  sorted = case boxOrder of
              BOByName -> sortOn fst asCurrentRunPropValues
              BOByVolume -> sortOn (Down . suVolume . snd) asCurrentRunPropValues
              BOByCount -> sortOn (Down . suCount . snd) asCurrentRunPropValues
              BOByShelve -> let -- find boxes in order of appearance
                  style'shelf :: Map Text (Text, Dimension)
                  style'shelf = Map.fromList $ reverse
                                             [ (boxStyle box , (sName shelfSum, boxOffset box))
                                             | baySum <- sDetailsList (currentRun state)
                                             , shelfSum <- sortOn sName $ sDetailsList baySum
                                             , box <- sortOn boxOffset $ map zCurrentEx $ sDetailsList shelfSum
                                             ]
                  in sortOn (flip lookup style'shelf . fst) asCurrentRunPropValues
-- * Find next shelf
-- | Find next shelf containing the given style
-- or highlighted run
findNextHLRun :: Text -> AppState -> AppState
findNextHLRun style AppState{..} = let
   nextRuns = drop (asCurrentRun + 1) (sDetails asShelvesSummary)
   indexM = V.findIndex (isJust . lookup style . sPropValues) nextRuns 
   newRun = case indexM of
               Nothing -> asCurrentRun
               Just i -> asCurrentRun + i + 1
   in AppState{asCurrentRun=newRun,..}


findPrevHLRun :: Text -> AppState -> AppState
findPrevHLRun style AppState{..} = let
   nextRuns = take (asCurrentRun) (sDetails asShelvesSummary)
   indexM = V.findIndex (isJust . lookup style . sPropValues) $ reverse nextRuns 
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
  
-- * Find  nex Box
findNextBox :: SearchDirection -> (Box RealWorld  -> Bool) -> AppState -> (AppState, Maybe (Box RealWorld))
findNextBox direction good state =
  let next = case direction of
                  Forward -> nextBoxThrough
                  Backward -> prevBoxThrough
  in case next state of
     new | coordinate new /= coordinate state , Just nextBox <- currentBox new -> 
           if good nextBox
           then (new, Just nextBox)
           else findNextBox direction good new
     new | coordinate new /= coordinate state , Nothing <- currentBox new -> 
       findNextBox direction good new
     _ -> (state , Nothing)
  where coordinate s = ( asCurrentRun s
                       , asCurrentBay s
                       , asCurrentShelf s
                       , asCurrentBox s
                       )

-- * Post update
-- | update the list of current styles
runUpdated :: AppState -> AppState
runUpdated state@AppState{..} = setBoxOrder asBoxOrder $ AppState{asCurrentRunPropValues=styles,..} where
    styles = fromList $ Map.toList $ sPropValues (currentRun state)
-- *  Run
runsSideBar :: AppState -> B.Widget Text
runsSideBar state@AppState{..} = B.renderTable $ runsToTable (currentPropValue state) (asHistoryRange state) (asViewMode state) asCurrentRun asShelvesSummary 

-- * PropValues
stylesSideBar :: AppState -> B.Widget Text
stylesSideBar state@AppState{..} = 
  B.renderTable $ stylesToTable (selectedPropValue state) asCurrentPropValue $ fmap fst asCurrentRunPropValues
-- renderStatus :: AppState -> Widgets
renderStatus state@AppState{..} = let
  mode = B.str (show asSummaryView)
  legend = B.hBox [ B.withAttr (percToAttrName r 0) (B.str [eigthV i]) | i <- [0..9] , let r = fromIntegral i / 8 ]
  in B.vLimit 1 $ B.hBox $ [ B.txt (sName $ currentShelf state)  -- current shelf
                           , B.center $ maybe (B.str "∅") styleNameWithAttr (asSelectedPropValue ) -- current style
                           , B.center $ maybe (B.str "∅") (B.txt . sText) (asBoxSelection ) -- current selection
                           , B.center $ B.str "/" B.<+> maybe (B.str "∅") (B.txt . sText) (asShelfSelection ) -- current selection
                           , B.center $ B.str (show asBoxOrder)
                           , B.center mode
                           , surroundIf (not asNavigateCurrent) $ B.txt $ displayEvent asDiffEvent
                           , if asNavigateWithPrevious then B.str "~" else B.emptyWidget
                           , surroundIf asNavigateCurrent $ B.str $ show (whCurrentEvent asWarehouse)
                           , B.padLeft B.Max legend
                           ]
  where surroundIf False w = w
        surroundIf True  w = B.hBox [B.str "[", w, B.str "]"]
             
debugShelf :: AppState -> B.Widget Text
debugShelf state = let
  ssum = currentShelf state
  in B.vBox $
     if -- | ViewSummary SVSurfaceLH <- asViewMode state 
        -- -> [renderHorizontalSummary bayToBars (currentRun state) ]
        | ViewSummary sview <- asViewMode state 
        -> renderHorizontalSummary (B.padTop B.Max . bayToBars sview) (currentRun state)  :
           [  B.hBox $ intersperse (B.str " ") 
                     $ [  B.str (show m)
                       , B.txt $ sName ssum
                       , renderS m ssum
                       , B.str $ printf "%02.0f%%" (ratio(fromSummary m) ssum * 100)
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
renderRun :: (Dimension -> Dimension) -> (ZHistory1 Box RealWorld -> B.Widget n) -> Run SumVec (SumVec (ZHistory1 Box RealWorld)) -> [ B.Widget n ]
renderRun project renderBox run =  concat 
          [ map (B.padTop B.Max)
            [ B.withAttr (fst bayNameAN )
                             $ B.vBox (map (withHLStatus (seBoxHLStatus $ sExtra bay) . B.str . pure) 
                             $ toList (sName bay <> "▄"))
                             --                     ^^^ aligned with the bottom border of the shelf
            , B.renderTable
            . baySummaryToTable project renderBoxes
            $ bay
            ]
          | bay <- F.toList . sDetails $ run
          ]
     where renderBoxes = \case
              [] -> B.emptyWidget
              [box] -> renderBox box
              boxes -> B.hBox $ B.str "(" : map renderBox boxes <> [ B.str ")" ]
          
-- * 
withHLBoxAttr :: AppState -> (Box RealWorld -> B.Widget n) ->  Box RealWorld -> B.Widget n
withHLBoxAttr state f box = withHLStatus (boxHLStatus state box) (f box)


boxHLStatus :: AppState -> Box RealWorld -> HighlightStatus
boxHLStatus state box = HighlightStatus{..} where
    hsCurrent = Just box == currentBox state
    hsHighlighted = if Just (boxPropValue box) == selectedPropValue state then 1 else 0
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

setProperty :: Text -> B.EventM Resource AppState ()
setProperty value = do
   let valuem = if null value then Nothing else Just value
   modify \s -> s { asProperty = valuem }
   execute (return ())

-- | Set a tag to all boxes (not only selected ones)
setTagAll :: Text -> B.EventM n AppState ()
setTagAll tag = execute do
       boxIds <- gets boxes
       boxes_ <- mapM findBox boxIds 
       let tagOps = parseTagOperations tag
       void $ zipWithM (updateBoxTags tagOps) (toList boxes_) [1..]

setBoxTitle title = setTagAll ("ctitle=" <> title)
   

makeBoxSelection :: Text -> WH (Maybe (Selection BoxSelector (BoxId s))) s
makeBoxSelection "" = return Nothing
makeBoxSelection sText = do 
   let sSelector = parseBoxSelector sText
   boxeIds <- findBoxByNameAndShelfNames sSelector
   let sSelected = Set.fromList $ map boxId boxeIds 
   return $ Just Selection{..}

makeShelfSelection :: Text -> WH (Maybe (Selection ShelfSelector Text)) s
makeShelfSelection "" = return Nothing
makeShelfSelection sText = do 
   let sSelector = parseShelfSelector sText
   shelfIds <- findShelvesByBoxNameAndNames sSelector
   shelves <- mapM findShelf shelfIds
   let sSelected = Set.fromList $ map shelfName shelves
   return $ Just Selection{..}
             
makeInputData :: InputMode -> AppState -> InputData
makeInputData imode state = let
    idInitial = case imode of 
                  ISelectProperty -> "$"
                  ISelectTag -> "ctitle=$"
                  _ -> ""
    idShelf =  sName $ currentShelf state
    idPropertyValue = fromMaybe "" $ selectedPropValue state
    idStyle = maybe "" boxStyle $ currentBox state
    idContent = case fmap boxContent $ currentBox state of
                     Just content | not (null content) -> "#'" <> content
                     _ -> ""
    idBoxSelector = maybe "" sText $ asBoxSelection state
    idShelfSelector = maybe "" sText $ asShelfSelection state
    idHistory = findWithDefault [] imode (asInputHistory state)
    in InputData{..}

                                                                     

                                                                     
yankOrEdit :: (Maybe Text) -> Text -> IO ()
yankOrEdit extm text = do
   let filePath = "/tmp/whp-text" <> (unpack $ fromMaybe "" extm)
   writeFileUtf8 filePath text
   void $ case extm of 
             Nothing -> rawSystem  "xclip" ["-i", "-selection", "clipboard", filePath]
             Just _ -> rawSystem  "xdg-open" [filePath]
