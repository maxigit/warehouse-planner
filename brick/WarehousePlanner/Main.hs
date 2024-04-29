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
import Options.Applicative
import Data.Text.IO qualified as Text
import Control.Monad.State (get)
import System.FilePath (takeBaseName)

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
            }
     deriving (Show, Generic)
     

data Command = Summary
             | Stocktake 
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
             | Report
             | Display 
             | Export
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

  return Options{..}
  
commandArg = flag' Stocktake (long "stocktake"
                             <> short 'k'
                             <> help "Generates a stocktake file with the final position of selected boxes"
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
          <|> flag' AllBoxes (long "all" <> long "all-boxes"
                             <> short 'a'
                             <> help "Number of boxes in all shelves"
                             )
          <|> flag' MopLocation (long "mop" <> long "mop-locations"
                             <> short 'm'
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
                                   <> help "Generic report. Can be selected with PARAM"
                                   )
  
optionsPI :: ParserInfo Options
optionsPI = info (helper <*> optionsParser ) fullDesc

-- * Main
defaultMain ::  IO ()
defaultMain = defaultMainWith (importDispatchDef)
defaultMainWith :: (FilePath -> Section -> IO (Either Text [Section])) -> IO ()
defaultMainWith expandSection = do
  o@Options{..} <- execParser optionsPI
  today <- case oToday of
                Nothing -> utctDay <$> getCurrentTime
                Just date -> return date
  let dir = fromMaybe "." oDir
      title = intercalate "-" $ map takeBaseName oFiles
      withHistory = oCommand `elem` [BoxHistory, Display]
                  && not oNoHistory
  scenarioE <- readScenarioFromPaths withHistory (expandSection dir) oDir oFiles
  extraScenarios <- mapM (readScenario $ expandSection dir) $ extraScenariosFrom o
  case sequence (scenarioE: extraScenarios) of
    Left e -> error $ unpack e
    Right scenarios -> do 
          let scenario = mconcat scenarios
          let boxSelectorM = fmap parseBoxSelector oParam
          let exec :: forall a . WH a RealWorld -> IO a
              exec action = do
                   let ?cache = noCache
                       ?today = today
                   warehouse <- execScenario scenario
                   execWH warehouse action
          let withLines linesWH = do
                 ls <- exec linesWH
                 void $ mapM outputText ls
              outputText = Text.putStrLn

          case oCommand of
            Summary -> exec summary >>= outputText . pack . show
            Display -> exec get >>= whMain title
            Stocktake -> withLines (generateStockTakes boxSelectorM)
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
            BoxHistory -> withLines (generateBoxHistory boxSelectorM)
            Export -> do
                        let bare = scenario { sInitialState = Nothing
                                            , sSteps = filter (\(Step h _ _) -> h `elem` [LayoutH, ShelvesH, OrientationsH, ShelfTagsH, ShelfSplitH, ShelfJoinH]) $ sSteps scenario
                                            , sLayout = sLayout scenario
                                            , sColourMap = sColourMap scenario
                                            }
                        scenarioToFullText bare >>= outputText
                        withLines $ generateStockTakes boxSelectorM


extraScenariosFrom :: Options -> [Text]
extraScenariosFrom Options{..} = mapMaybe (fmap unlines) [deleteM, importM, tamM] where
    deleteM = flip fmap oDelete \del -> [ ":DELETE:" , del , ":END:" ]
    importM = flip fmap oImport \imp -> [ ":IMPORT:" , imp , ":END:" ]
    tamM = flip fmap oTagsAndMoves \tam -> [ ":Tags and Moves:" , "stock_id,tam" , tam , ":END:" ]
                                 


  
main = defaultMain
