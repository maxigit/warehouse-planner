module WarehousePlanner.WPL.Exec
( runWPL
, readWPL
)
where 

import ClassyPrelude
import WarehousePlanner.WPL.Types
import WarehousePlanner.Base
import Text.Megaparsec qualified as P
import WarehousePlanner.WPL.Parser
import WarehousePlanner.WPL.ExContext


runWPL :: [Statement] -> WH () s
runWPL statements = do
   i <- getInitial
   mapM_ (void . executeStatement i) statements

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
    where execCase ec (Case com comm) = do
             newEc <- executeStatement ec com
             forM comm (executeStatement newEc)
             return $ inverseBoxes newEc

         




executeCommand :: ExContext s -> Command -> WH (ExContext s) s
executeCommand ec command = case command of
    Move boxm shelfm -> do
      boxes <- getBoxes <$> case boxm of 
                 Nothing -> return ec
                 Just sel -> narrowBoxes sel ec
      shelves <- getShelves <$> case shelfm of
                    Nothing -> return ec
                    Just sel -> narrowShelves sel ec
      leftOver <- moveBoxes ExitLeft PRightOnly DontSortBoxes boxes shelves
      return $ applyExOperation (ExcludeBoxes leftOver) ec
    SelectBoxes selector -> do
      narrowBoxes selector ec
    SelectShelves selector -> do
      narrowShelves selector ec
  

readWPL :: MonadIO m => FilePath ->  m [Statement]
readWPL filename = do 
    content <- readFileUtf8 filename
    let e = P.runParser wplParser filename content
    case e of
       Left bundle -> error $ P.errorBundlePretty bundle
       Right statements -> return statements

