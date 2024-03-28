{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ImplicitParams #-}
module WarehousePlanner.Main 
( defaultMain
, main
)
where

import ClassyPrelude 
import WarehousePlanner.Base
import WarehousePlanner.Org
import WarehousePlanner.Brick.App
import WarehousePlanner.Report
import Options.Applicative
import Data.Text.IO qualified as Text
import Control.Monad.State (get)

-- * Type
data Options = Options
            { oFiles :: [FilePath]
            , oCommand :: Command
            , oParam :: Maybe Text 
            , oToday :: Maybe Day
            , oDir :: Maybe FilePath
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
             | Report
             | Display 
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
  oFiles <- many (argument str $ metavar "FILES"
                               <> help "Org files without suffixes"
                 )
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
          <|> flag' Expand (long "expand" <> long "export"
                           <> short 'x'
                           <> help "Expands full scenario."
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
  Options{..} <- execParser optionsPI
  today <- case oToday of
                Nothing -> utctDay <$> getCurrentTime
                Just date -> return date
  scenarioE <- readScenarioFromPaths (expandSection $ fromMaybe "." oDir) oDir oFiles
  case scenarioE of
    Left e -> error $ unpack e
    Right scenario -> do 
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
            Display -> exec get >>= whMain
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




  
main = defaultMain
