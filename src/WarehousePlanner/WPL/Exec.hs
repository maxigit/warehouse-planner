module WarehousePlanner.WPL.Exec
( runWPL
, runWPLWith
, readWPL
, parseWPL
)
where 

import ClassyPrelude
import WarehousePlanner.WPL.Types
import WarehousePlanner.Base
import WarehousePlanner.Move
import WarehousePlanner.Selector (printBoxSelector, parseBoxSelector)
import Text.Megaparsec qualified as P
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.ExContext
import Control.Monad(zipWithM_)


runWPL :: [Statement] -> WH () s
runWPL = runWPLWith (const $ return ())

runWPLWith :: (Maybe (ExContext s) -> WH r s) -> [Statement] -> WH r s
runWPLWith action statements = do
  ecs <- mapM (executeStatement withAll) statements
  action $ lastMay ecs

executeStatement :: ExContext s -> Statement -> WH (ExContext s) s
executeStatement ec command = 
    case command of 
        Action command -> do
           executeCommand ec command 
        Then a b -> do
          executeStatement ec a >>= flip executeStatement b
        Ors coms -> do
            mapM (void . executeStatement ec) coms
            return ec
        Cases cs -> do
           foldM execCase ec cs
        PassThrought statement -> do
           executeStatement ec statement
           return ec
    where execCase ec (Case com comm) = do
             newEc <- executeStatement ec com
             forM comm (executeStatement newEc)
             return $ inverseShelves $ inverseBoxes newEc

         




executeCommand :: ExContext s -> Command -> WH (ExContext s) s
executeCommand ec command = case command of
    --------
    Move boxm shelf -> do
      newBaseEvent "TO" (maybe "" printBoxSelector   boxm <> " -> " <> pack (showCSelector showShelfSelector shelf))
      boxes <- getBoxes =<< case boxm of 
                 Nothing -> return ec
                 Just sel -> narrowBoxes sel ec
      shelves <-  do
         getShelves =<< narrowCSelector narrowShelves shelf ec
      inEx <- moveBoxes ExitLeft PRightOnly DontSortBoxes boxes shelves
      return ec { ecBoxes = fmap ((,error "boom") . boxId) inEx }
    ---------
    Tag tagOps -> do
      newBaseEvent "TAG" (tshow tagOps)
      boxes <- getBoxes ec
      zipWithM_ (updateBoxTags tagOps) boxes [1..]
      return ec
    ---------
    ToggleTags tagOps -> do
      newBaseEvent "TOGGLE" (tshow tagOps)
      boxes <- getBoxes ec
      excluded <- getBoxes (inverseBoxes ec)
      zipWithM_ (updateBoxTags tagOps) boxes [1..]
      zipWithM_ (updateBoxTags $ negateTagOperations tagOps) excluded [1..]
      return ec
    ---------
    SelectBoxes selector -> do
      narrowCSelector narrowBoxes selector ec
    ---------
    SelectShelves selector -> do
      narrowCSelector narrowShelves selector ec
    ---------
    TagAndMove txt ors -> do
      let (tags, locm) = splitTagsAndLocation txt
      inEx <- moveAndTag ec [] (parseBoxSelector "*" , tags, locm, ors)
      return ec { ecBoxes = fmap ((,error "boom2") . boxId) inEx }
      
    ---------
  

readWPL :: MonadIO m => FilePath ->  m [Statement]
readWPL filename = do 
    content <- readFileUtf8 filename
    let e = P.runParser wplParser filename content
    case e of
       Left bundle -> error $ P.errorBundlePretty bundle
       Right statements -> return statements

parseWPL :: FilePath -> Text -> Either  Text [Statement]
parseWPL source content =
    case P.runParser wplParser source content of
       Left bundle -> Left $ pack $ P.errorBundlePretty bundle
       Right statements -> return statements


narrowCSelector :: (selector -> ExContext s -> WH (ExContext s) s) -> CSelector selector -> ExContext s -> WH (ExContext s) s
narrowCSelector narrow cselector ec = 
   case cselector of
       CSelector selector -> narrow selector ec
       Parent -> return $ fromMaybe withAll (ecParent ec)
       Root -> return $ withAll --  . show $  up $ error $ show ec
       SwapContext -> return $ inverseBoxes ec 
       CStatement stmt -> executeStatement ec stmt
   -- where up e = maybe e up (ecParent e)
