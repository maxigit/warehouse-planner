{-# OPTIONS_GHC -Wno-deprecations #-}
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
import Data.Set qualified as Set


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
           -- "select" each option and pass the leftover to the next option
           -- (so that already selected boxes are excluded for the other cases)
           -- However, the selected option are collected in order and passed at the end
           -- ex
           -- if we have A B C D E
           --   & | A                 select A leftover B C D E -> included: B C D E  excluded: A
           --     | C                 select C leftover B D E   -> included: B D E    excluded: A C
           --     | A                 select 0 leftover B D E   -> included: B D E    excluded: A C
           --     | D                 select D leftover B E     -> included: B E      excluded A C D
           --   &                    included A C D    excluded B E
           inverseBoxes <$> foldM execCase ec cs
        PassThrought statement -> do
           executeStatement ec statement
           return ec
        ForeachShelf statement -> do
           -- set the context to each individial shelf and 
           -- Maybe we should use the parent content
           let shelves = includedList $ ecShelves ec
           forM_ shelves \shelf -> 
                void $ executeStatement (ec { ecShelves = (ecShelves ec) {included = Just [shelf] }}) statement
           return ec

           
    where execCase ec (Case com comm) = do
             newEc <- executeStatement ec com
             forM comm (executeStatement newEc)
             let newBoxes = ecBoxes $ inverseBoxes newEc
             -- reinject previously selected boxes to exclude so that in effect, all selections
             -- are collected in the excluded
             return newEc { ecBoxes = newBoxes { excluded = excluded (ecBoxes ec) `merge` excluded newBoxes } }
             -- return $ inverseBoxes newEc
          merge (Just as) (Just bs) = Just (as <> bs)
          merge am bm = am <|> bm

         




executeCommand :: ExContext s -> Command -> WH (ExContext s) s
executeCommand ec command = case command of
    --------
    Move boxm pmodem orules shelf -> do
      newBaseEvent "TO" (maybe "" printBoxSelector   boxm <> " -> " <> pack (showCSelector showShelfSelector shelf))
      boxes <- getBoxPs =<< case boxm of 
                 Nothing -> return ec
                 Just sel -> narrowBoxes sel ec
      -- traceShowM ("BOXOS", length boxes)
      shelves <-  do
         getShelves =<< narrowCSelector shelf ec
      -- traceShowM ("SHELVES", shelf, length shelves)
      let rules = case orules of
                    [] -> ecOrientationStrategies ec
                    _ -> orules
                  
      inEx <- withBoxOrientations rules $ moveSortedBoxes ExitLeft (fromMaybe (ecPartitionMode ec) pmodem) boxes shelves
      return ec { ecBoxes = fmap (first boxId) inEx }
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
      narrowCSelector selector ec
    ---------
    SelectShelves selector -> do
      narrowCSelector selector ec
    ---------
    TagAndMove txt ors0 -> do
      let (tags, locm) = splitTagsAndLocation txt
          ors = case ors0 of
                     [] -> ecOrientationStrategies ec
                     _ -> ors0
      inEx <- moveAndTag ec [] (parseBoxSelector "*" , tags, locm, ors)
      return ec { ecBoxes = fmap (first boxId) inEx }
    ---------
    Delete -> do
       boxes <- getBoxes ec
       deleteBoxes boxes
       return ec {ecBoxes = InExcluded (Just []) (Just []) }
    ---------
    TraceCount desc-> do
       let ExContext{..} = ec
       traceM $ "Trace Count " <> unpack desc -- <> " "  <> show ecSelector
       traceM $ "     included boxes " <> show(  fmap length $ included ecBoxes )
       traceM $ "     excluded boxes " <> show(  fmap length $ excluded ecBoxes)
       traceM $ "     included shelves " <> show( fmap length $ included ecShelves)
       traceM $ "     excluded shelves " <> show( fmap length $ excluded ecShelves)
       return ec
    TraceBoxes desc -> do
       let ExContext{..} = ec
       traceM $ "Trace Boxes " <> unpack desc -- <> " "  <> show ecSelector
       incs <- mapM (findBox . fst) `traverse` (included ecBoxes)
       exs <- mapM (findBox . fst) `traverse` (excluded ecBoxes)
       traceM $ "     included boxes " <> show(  map boxStyleAndContent <$> incs)
       traceM $ "     excluded boxes " <> show(  map boxStyleAndContent <$> exs)
       return ec
    TraceShelves desc -> do
       let ExContext{..} = ec
       traceM $ "Trace Shelves " <> unpack desc -- <> " "  <> show ecSelector
       incs <- mapM findShelf `traverse` included ecShelves
       exs <- mapM findShelf `traverse` excluded ecShelves
       traceM $ "     included shelves " <> show(  map shelfName <$> incs)
       traceM $ "     excluded shelves " <> show(  map shelfName <$> exs)
       return ec
    ---------
    SetPartitionMode pmode -> do
       return ec { ecPartitionMode = pmode }
    ---------
    SetOrientationStrategies os -> do
       return ec { ecOrientationStrategies = os }
    ---------
    SetNoEmptyBoxes emptyBoxes -> do
       return ec { ecNoEmptyBoxes = emptyBoxes }
    ---------
    SetNoEmptyShelves emptyShelves -> do
       return ec { ecNoEmptyShelves = emptyShelves }



      
       
  

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

class Narrowing selector where
  narrow :: selector -> ExContext s -> WH (ExContext s) s
  useContext :: ExContext s -> WH selector s
  
instance Narrowing BoxSelector where
  narrow = narrowBoxes
  useContext ec = do
    -- narrow using context shelves
    case included (ecShelves ec) of 
      Nothing -> return selectAllBoxes
      Just _ -> do
         shelves <- getShelves ec
         let shelfSelectors = Selector (NameMatches $ map (MatchFull . shelfName) shelves) []
         return selectAllBoxes { shelfSelectors }
  
instance Narrowing ShelfSelector where
  narrow = narrowShelves
  useContext ec = do
     case included (ecBoxes ec) of 
         Nothing -> return $ selectAllShelves
         Just _ -> do
           boxes <- getBoxes ec
           shelves <- mapM findShelf $ mapMaybe boxShelf boxes
           let shelfList = Set.fromList shelves
               sShelfSelectors = Selector (NameMatches $ map (MatchFull . shelfName) $ toList shelfList) []
           return selectAllShelves {sShelfSelectors}

narrowCSelector :: forall selector s . Narrowing selector => CSelector selector -> ExContext s -> WH (ExContext s) s
narrowCSelector cselector ec = do
   ec' <- case cselector of
       CSelector selector -> narrow selector ec
       Parent -> return $ fromMaybe withAll (ecParent ec)
       Root -> return $ withAll --  . show $  up $ error $ show ec
       SwapContext -> return $ inverseBoxes ec 
       CStatement stmt -> executeStatement ec stmt
       CUseContext ->  do
          selector :: selector <- useContext ec
          narrow selector ec
       CSelectorAnd s1 s2 -> narrowCSelector s1 ec >>= narrowCSelector s2
   return ec' { ecNoEmptyBoxes = ecNoEmptyBoxes ec, ecNoEmptyShelves = ecNoEmptyShelves ec }

   -- where up e = maybe e up (ecParent e)
