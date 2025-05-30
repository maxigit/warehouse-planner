{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImplicitParams #-}

module WarehousePlanner.Main 
( defaultMain
, defaultMainWith
, main
)
where

import ClassyPrelude 
import WarehousePlanner.Base
import WarehousePlanner.Org
import WarehousePlanner.Selector
import WarehousePlanner.Brick.App
import WarehousePlanner.Report
import WarehousePlanner.Csv (readColourMap)
import Options.Applicative
import Data.Text.IO qualified as Text
import Control.Monad.State (get)
import System.FilePath (takeBaseName)
import WarehousePlanner.Brick.Types
import Data.Map qualified as Map
import WarehousePlanner.WPL.PrettyPrint (prettyWPLs)
import WarehousePlanner.WPL.Exec(parseWPL)

-- * Type
data Options = Options
            { oFiles :: [FilePath]
            , oCommand :: Command
            , oParam :: Maybe Text 
            , oToday :: Maybe Day
            , oImport :: Maybe Text
            , oDelete :: Maybe Text
            , oTagsAndMoves :: Maybe Text
            , oDir :: Maybe FilePath
            , oNoHistory :: Bool
            , oProperty :: Maybe Text
            , oCurrentRun :: Maybe Int
            , oBoxSearch :: Maybe Text
            , oShelfSearch :: Maybe Text
            , oNoCheck :: Maybe Bool
            , oNoWatch :: Bool
            }
     deriving (Show, Generic)
     

data Command = Summary
             | Stocktake 
             | FuzzyStocktake 
             | Expand 
             | Moves
             | MopLocation
             | MovesWithTags
             | ShelvesReport
             | ShelvesGroupReport
             | AllBoxes
             | BestBoxesFor
             | BestShelvesFor
             | BoxGroupReport
             | BoxHistory
             | TagReport
             | Report
             | Display 
             | Export
             | PrettyPrintWPL
             deriving (Show, Eq, Generic, Read)
             
optionsParser :: Parser Options
optionsParser = do 
  oDir <- optional $ strOption $ long "dir" <> short 'd' <> metavar "DIR" <> help "Base directory"
  oCommand <- commandArg <|> pure Display
  oParam <- optional $ strOption $ long "param" <> short 'p'
                               <> metavar "PARAM"
                               <> help "Extra parameter usually box or shelf selector"
  oToday <- optional $ option auto $ long "today" <> long "date"
                                   <> short 'D'
                                   <> metavar "DATE"
                                   <> help "Changes today's date"
  oImport <- optional $ strOption $ long "import" 
                                  <> short 'i'
                                  <> metavar "IMPORT"
                                  <> help "import line as in :IMPORT: section"
  oDelete <- optional $ strOption $ long "delete"  <> long "del"
                                  <> metavar "SELECTOR"
                                  <> help "delete the selected boxes"
  oTagsAndMoves <- optional $ strOption $ long "tags-and-moves"  <> long "tam"
                                  <> metavar "SELECTOR,TagAndMove"
                                  <> help "tag and move selected boxes"
  oFiles <- many (argument str $ metavar "FILES"
                               <> help "Org files without suffixes"
                 )
  oNoHistory <- switch $ long "no-history"
                             <> help "DeActivate history"
  oProperty <- optional $ strOption $ long "property"
                                    <> metavar "PROPERTY"
                                    <> help "property to use highlight boxes differently"
  oCurrentRun <- optional $ option auto $ long "run"
                                        <> metavar "RUN NUMBER"
                                        <> help "Number of the run to start with (0 based)"
  oNoCheck <- asum [ flag' (Just True) ( long "no-check" <> help "Disable scenario checks")
                   , flag' (Just False) (long "check" <> help "add auto checks")
                   , pure Nothing
                   ]
  oBoxSearch <- optional $ strOption $ long "box-search"
                       <> help "box selector"
  oShelfSearch <- optional $ strOption $ long "shelf-search"
                        <> help "shelf selector"
  oNoWatch <- switch $ long "no-watch"
                   <> help "Watch for files in datadir"
  return Options{..}
  
commandArg = flag' Stocktake (long "stocktake"
                             <> short 'k'
                             <> help "Generates a stocktake file with the final position of selected boxes"
                             )
          <|>flag' FuzzyStocktake (long "fuzzy-stocktake"
                             <> short 'K'
                             <> help "Generates a stocktake file with the fuzzy position of selected boxes"
                             )
          <|> flag' Display (long "display"
                            <> help "Launch interactive viewer"
                            )
          <|> flag' Summary (long "Summary"
                            <> short 'y'
                            <> help "Displays summary statistics (volumen and % or use)"
                            )
          <|> flag' Expand (long "expand"
                           <> short 'X'
                           <> help "Expands full scenario.\nScenario with IMPORT sections expanded to their results"
                           )
          <|> flag' Export (long "export"
                           <> short 'x'
                           <> help "Export full scenario.\nReady to use : shelves + layout + stock with positions"
                           )
          <|> flag' Moves (long "moves"
                          <> short 'm'
                          <> help "Boxes with location"
                          )
          <|> flag' MovesWithTags (long "tags"
                          <> short 't'
                          <> help "Boxes with location and tags"
                          )
          <|> flag' ShelvesReport (long "shelves" <> long "shelves-report"
                          <> short 's'
                          <> help "list shelves dimension and main content"
                          )
          <|> flag' ShelvesGroupReport (long "shelves-group" <> long "shelves-group-report"
                          <> short 'S'
                          <> help "Group shelves by similar dimension"
                          )
          <|> flag' BoxGroupReport (long "boxes-group" <> long "boxes-group-report"
                          <> short 'A' <> short 'g'
                          <> help "Group boxes"
                          )
          <|> flag' BoxHistory (long "box-history" <> long "boxes-history-report"
                          <> help "Box History"
                          )
          <|> flag' TagReport (long "tag-values"  
                          <> help "tags and their values"
                          )
          <|> flag' AllBoxes (long "all" <> long "all-boxes"
                             <> short 'a'
                             <> help "Number of boxes in all shelves"
                             )
          <|> flag' MopLocation (long "mop" <> long "mop-locations"
                             <> help "Location files compatible with MOP"
                             )
          <|> flag' BestBoxesFor (long "best-boxes" <> long "best-boxes-for"
                             <> short 'B'
                             <> help "Best boxes to fill the selected shelves (PARAM)"
                             )
          <|> flag' BestShelvesFor (long "best-shelves" <> long "best-shelves-for"
                                   <> short 'b'
                                   <> help "Best shelves to fill the selected boxes (PARAM)"
                                   )
          <|> flag' Report (long "report" 
                                   <> short 'r'
                                   <> help "Generic report. Can be selected with PARAM. Doesn't add 'report-' prefix."
                                   )
          <|> flag' PrettyPrintWPL ( long "pretty"
                                   <> help "pretty print WPL from stdin"
                                   )
  
optionsPI :: ParserInfo Options
optionsPI = info (helper <*> optionsParser ) fullDesc

-- * Main
defaultMain ::  IO ()
defaultMain = defaultMainWith (importDispatchDef)
defaultMainWith :: (FilePath -> Int -> Section -> IO (Either Text [Section])) -> IO ()
defaultMainWith expandSection = do
  o@Options{..} <- execParser optionsPI
  today <- case oToday of
                Nothing -> utctDay <$> getCurrentTime
                Just date -> return date
  let dir = fromMaybe "." oDir
      title = intercalate "-" $ map takeBaseName oFiles
      withHistory = oCommand `elem` [BoxHistory, Display]
                  && not oNoHistory
      getExec :: forall a . IO (Either Text (WH a RealWorld -> IO a, Scenario))
      getExec = do
                    scenarioE <- readScenarioFromPaths withHistory (expandSection dir) oDir oFiles
                    extraScenarios <- case extraScenariosFrom o of
                                        [] -> return []
                                        extras -> do 
                                             ss <- mapM (readScenario (expandSection dir) Nothing) extras
                                             return $ Right (mempty { sSteps = [ NewFile  "<ARGUMENTS>" ] }) : ss
                    case sequence (scenarioE: extraScenarios) of
                         Left e -> return $ Left e
                         Right scenarios -> do 
                          let scenario = filterChecks $ mconcat scenarios
                              filterChecks = if oNoCheck == Just True
                                             then \scen -> scen {sSteps = filter (not . isCheck) $ sSteps scen }
                                             else id
                              isCheck s = case s of 
                                           Step CheckShelvesH _ _ -> True
                                           _ -> False
                          let exec :: forall a . WH a RealWorld -> IO a
                              exec action = do
                                   let ?cache = if withHistory then refCache else noCache
                                       ?today = today
                                   warehouse <- execScenario scenario
                                   execWH warehouse action
                          return $ Right (exec, scenario)
      outputText = Text.putStrLn
  case oCommand of
       Summary -> do 
               -- workaround exec having a monomorphic type
               execE <- getExec
               case execE of
                 Left e -> error $ unpack e
                 Right (exec, _) -> exec summary >>= \(ls, footer) -> mapM_ outputText (map (intercalate ",") $ ls ++ [footer])
       Display -> let 
               setParam state = do
                        (join -> asBoxSelection) <- mapM makeBoxSelection oBoxSearch
                        (join -> asShelfSelection) <- mapM makeShelfSelection oShelfSearch
                        return state { asProperty = oProperty 
                                               , asInputHistory = case oProperty of
                                                                    Nothing -> id
                                                                    Just prop -> \m ->  Map.singleton ISelectProperty [prop] <> m
                                                                  $ asInputHistory state
                                               , asCurrentRun = fromMaybe 0 oCurrentRun
                                               , asBoxSelection
                                               , asShelfSelection
                                               }
               watchPath = if oNoWatch
                           then Nothing
                           else Just (fromMaybe "." oDir)
               in whMain setParam title watchPath do
                      execE <- getExec
                      case execE of
                        Right (exec, scenario) -> do 
                                          contentPath <- contentPathM
                                          colourMaps <- liftIO $ mapM (readColourMap . contentPath) (sColourMap scenario)
                                          fmap (Right . (, mconcat $ reverse colourMaps) ) $  exec get
                        Left e -> return $ Left e
       PrettyPrintWPL -> do
          content <- case oFiles of
                        [one] -> readFileUtf8 $ dir </> one
                        [] -> repack <$> getContents
                        _ -> error "Only one WPL file can be pretty printed!"
          case parseWPL "<stdin>" $ repack content of
             Left e -> putStrLn e
             Right statements -> putStrLn $ prettyWPLs statements
       _ -> do
        execE <- getExec
        case execE of
          Left e -> error $ unpack e
          Right (exec, scenario) -> do 
                let withLines linesWH = do
                       ls <- exec linesWH
                       void $ mapM outputText ls
                let boxSelectorM = fmap parseBoxSelector oParam
                case oCommand of
                  Stocktake -> withLines (generateStockTakes boxSelectorM)
                  FuzzyStocktake -> withLines (generateFuzzyStockTakes boxSelectorM)
                  Expand -> scenarioToFullText scenario >>= outputText
                  MovesWithTags -> withLines (generateMoves DontSortBoxes boxSelectorM (boxStyleWithTags))
                  Moves -> withLines (generateMoves DontSortBoxes boxSelectorM boxStyle)
                  ShelvesReport -> withLines shelvesReport
                  ShelvesGroupReport -> withLines groupShelvesReport
                  AllBoxes -> withLines reportAll
                  MopLocation -> withLines (generateMOPLocations boxSelectorM)
                  BestBoxesFor -> withLines (bestBoxesFor (fromMaybe "" oParam))
                  BestShelvesFor -> withLines (bestShelvesFor (fromMaybe "" oParam))
                  Report -> withLines (generateGenericReport today (fromMaybe "report" oParam))
                  BoxGroupReport -> withLines do
                                    boxes <- findBoxByNameAndShelfNames (fromMaybe (parseBoxSelector "") boxSelectorM)
                                    groupBoxesReport boxes
                  BoxHistory -> withLines (generateBoxesHistory boxSelectorM)
                  TagReport -> withLines (generateTags boxSelectorM)
                  Export -> do
                              let bare = scenario { sInitialState = Nothing
                                                  , sSteps = filter (\case
                                                                      (Step h _ _) -> h `elem` [LayoutH, ShelvesH, OrientationsH, ShelfTagsH, ShelfSplitH, ShelfJoinH]
                                                                      _ -> False
                                                                    )
                                                                    $ sSteps scenario
                                                  , sLayout = sLayout scenario
                                                  , sColourMap = sColourMap scenario
                                                  }
                              scenarioToFullText bare >>= outputText
                              withLines $ generateStockTakes boxSelectorM


extraScenariosFrom :: Options -> [Text]
extraScenariosFrom Options{..} = mapMaybe (fmap unlines) [deleteM, importM, tamM, checkM] where
    deleteM = flip fmap oDelete \del -> [ ":DELETE:" , del , ":END:" ]
    importM = flip fmap oImport \imp -> [ ":IMPORT:" , imp , ":END:" ]
    tamM = flip fmap oTagsAndMoves \tam -> [ ":Tags and Moves:" , "stock_id,tam" , tam , ":END:" ]
    checkM = case oNoCheck of
                 Just False -> Just [ ":CHECK_SHELVES:", "shelves", "#@check=-skip", ":END:" ]
                 _ -> Nothing
                                 


  
main = defaultMain
