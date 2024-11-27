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
import WarehousePlanner.ShelfOp
import Control.Monad(zipWithM_, zipWithM)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Control.Monad.State(gets)
import Data.List (nub)
import Data.Proxy


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
           let subEc = ec { ecParent = Just ec
                          , ecBoxes = (ecBoxes ec) { excluded = Just [] }  -- clear exclude so that we don't get back boxes from the original context
                          } 
           new <- inverseBoxes <$> foldM execCase  subEc cs
           return new
        ShelfCases cs -> do
           inverseShelves <$> foldM execShelfCase ec cs
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
        ForeachBox selector statement -> do
           boxes <- narrowCSelector selector ec >>= getBoxPs
           -- group by "global", no sorting
           let groups = groupBy (\a b -> key a == key b) boxes
               key (_, (global,_)) = global
           forM_ groups \box'ps -> do
                let idSet = Set.fromList $ map (boxId . fst) box'ps
                void $ executeStatement ec { ecBoxes = narrowIncluded (\(bId, _) -> bId `member` idSet)
                                                                      (ecBoxes ec)
                                           }
                                        statement
           return ec
        ForeachDo action statements  -> do
           -- execute action for all of the statment
           -- collecting boxes as in a case 
           let cases = fmap (\st -> Case st $ Just action) statements
           executeStatement ec (Cases cases)
           
    where execCase ec (Case com comm) = do
             newEc <- executeStatement ec com
             forM comm (executeStatement newEc)
             -- reinject previously selected boxes to exclude so that in effect, all selections
             -- are collected in the excluded
             let toExclude = (`notMember` (Set.fromList $ map fst $ includedList $ ecBoxes newEc)) . fst
             return  ec { ecBoxes = narrowIncluded   toExclude (ecBoxes ec) }
          execShelfCase ec (ShelfCase com comm) = do
             newEc <- executeStatement ec com
             forM comm (executeStatement newEc)
             let toExclude = (`notMember` (Set.fromList $ includedList $ ecShelves newEc))
             -- reinject previously selected shelves to exclude so that in effect, all selections
             -- are collected in the excluded
             return ec { ecShelves = narrowIncluded toExclude (ecShelves ec) }

         




executeCommand :: forall s . ExContext s -> Command -> WH (ExContext s) s
executeCommand ec command = case command of
    --------
    Move boxm pmodem orules shelf exitMode -> do
      newBaseEvent "TO" (maybe "" printBoxSelector   boxm <> " -> " <> pack (showCSelector showShelfSelector shelf))
      boxes <- getBoxPs =<< case boxm of 
                 Nothing -> return ec
                 Just sel -> narrowBoxes sel ec
      -- traceShowM ("BOXOS", length boxes)
      shelves <-  do
         getShelves =<< narrowCSelector shelf ec
      -- traceShowM ("SHELVES", shelf, length shelves)

      let rules = case orules of
                       _ -> (Nothing, orules) : ecOrientationStrategies ec

      inEx <- withBoxOrientations' rules $ moveSortedBoxes exitMode (fromMaybe (ecPartitionMode ec) pmodem) boxes shelves
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
    TagShelves tagOps -> do
       shelves <- getShelves ec
       forM shelves $ updateShelfTags tagOps
       return ec
    ---------
    SelectBoxes selector -> do
      narrowCSelector selector ec
    ---------
    SelectShelves selector -> do
      narrowCSelector selector ec
    ---------
    SelectBoxRanges boundary selector -> do
      selection <- narrowCSelector selector ec >>= getBoxPs
      boxId'Ps <- getBoxPs ec
      let getIds = map (first boxId)
      let ecB = case fromNullable selection of 
                     Nothing ->
                           let selectAll = InExcluded (Just $ getIds boxId'Ps) (Just [])
                               selectNone = InExcluded (Just []) (Just $ getIds boxId'Ps)
                           in case boundary of
                                   From  -> selectNone
                                   Before -> selectAll
                                   After -> selectNone
                                   Upto -> selectAll
                     Just nsel ->
                          let firstBox = fst $ head nsel
                              lastBox'p = last nsel
                              lastBox = fst $ lastBox'p
                              (before, from) =  break (\(b,_) -> b==firstBox) boxId'Ps
                              (upto', after') = break (\(b,_) -> b==lastBox) boxId'Ps
                              upto = upto' <> [lastBox'p]
                              after = drop 1 after'
                              (ins, outs) = case boundary of
                                               From -> (from, before)
                                               Before -> (before, from)
                                               Upto -> (upto, after)
                                               After -> (after, upto)
                          in InExcluded (Just $ getIds ins) (Just $ getIds outs)
      return $ ec { ecBoxes = ecB, ecParent = Just ec }
    ---------
    TagAndMove txt ors -> do
      let (tags, locm) = splitTagsAndLocation txt
      inEx <- withBoxOrientations' (ecOrientationStrategies ec) $ moveAndTag ec [] (parseBoxSelector "*" , tags, locm, ors)
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
    TraceBoxes withId desc -> do
       let ExContext{..} = ec
           boxTitle = if withId
                      then show
                      else unpack . boxStyleAndContent
       traceM $ "Trace Boxes " <> unpack desc -- <> " "  <> show ecSelector
       incs <- mapM (findBox . fst) `traverse` (included ecBoxes)
       exs <- mapM (findBox . fst) `traverse` (excluded ecBoxes)
       traceM $ "     included boxes " <> show (  map boxTitle <$> incs)
       traceM $ "     excluded boxes " <> show (  map boxTitle <$> exs)
       return ec
    TraceShelves desc -> do
       let ExContext{..} = ec
       traceM $ "Trace Shelves " <> unpack desc -- <> " "  <> show ecSelector
       incs <- mapM findShelf `traverse` included ecShelves
       exs <- mapM findShelf `traverse` excluded ecShelves
       traceM $ "     included shelves " <> show(  map shelfName <$> incs)
       traceM $ "     excluded shelves " <> show(  map shelfName <$> exs)
       return ec
    TraceOrientations desc -> do
       traceM $ "Trace orientation rules" <> unpack desc
       boxes <- getBoxes ec
       shelfves <- getShelves ec
       boxo <- gets boxOrientations
       -- try all box/shelf combination and group them by rules
       let withRules :: Map OrientationStrategy [(Box s, Shelf s)]
           withRules = Map.fromListWith (<>)
                               [(rule, [(box, shelf)])
                               | box <- boxes
                               , shelf <- shelfves
                               , rule <- boxo box shelf
                               ]
           displayM (rule, box'shelfs) = do
               let (bs,ss) = unzip box'shelfs
                   boxInfos = nub $ sort $ map (\b -> (boxStyle b, printDim $ boxDim b)) bs
                   shelfInfos = nub $ sort $ map shelfName ss
               traceM $ "---------- RULE: " <> unpack (showOrientationStratety rule)
               traceM $ "      BOXES: " <> show boxInfos
               traceM $ "      SHELVES: " <> show shelfInfos
       mapM_ displayM $ Map.toList withRules
       return ec
        
    ---------
    AssertBoxes assertNull desc -> do
       case included (ecBoxes ec) == Just [] of
           True | assertNull == False  -> do
                executeCommand ec (TraceCount $ "ASSERT BOXES " <> desc)
                error $ "no boxes null " <> show desc
           False | assertNull -> do
                executeCommand ec (TraceCount $ "ASSERT NO BOXES " <> desc)
                error $ "boxes present " <> show desc
           _  -> return ()
       return ec
    ---------
    AssertShelves assertNull desc -> do
       case included (ecShelves ec) == Just [] of
           True | assertNull == False  -> do
                executeCommand ec (TraceCount $ "ASSERT SHELVES " <> desc)
                error $ "no shelves null " <> show desc
           False | assertNull -> do
                executeCommand ec (TraceCount $ "ASSERT NO SHELVES " <> desc)
                error $ "shelves present " <> show desc
           _  -> return ()
       return ec
    ---------
    SetPartitionMode pmode -> do
       return ec { ecPartitionMode = pmode }
    ---------
    SetOrientationStrategies selector os -> do
       return ec { ecOrientationStrategies = [(selector, os)] }
    ---------
    AddOrientationStrategies selector os -> do
       return ec { ecOrientationStrategies = (selector, os) : ecOrientationStrategies ec }
    ---------
    SetNoEmptyBoxes emptyBoxes -> do
       return ec { ecNoEmptyBoxes = emptyBoxes }
    ---------
    SetNoEmptyShelves emptyShelves -> do
       return ec { ecNoEmptyShelves = emptyShelves }
    ---------
    ResizeShelf selector statement -> do -- full
        let full s = s { minDim = maxDim s }
        shelves <- narrowCSelector selector ec >>= getShelves
        forM shelves $ updateShelf full
        executeStatement ec statement <* forM shelves (\orig -> updateShelf (\s -> s { minDim = minDim orig }) orig )
    ResizeBox mode selector statement -> do 
        boxes <- narrowCSelector selector ec >>= getBoxes
        let around action = case fromNullable $ map _boxDim boxes of
                                 Nothing -> action
                                 Just dims -> do
                                      let dim = case mode of 
                                                     MaxDimension -> maxDimension $ toList dims
                                                     MinDimension -> minDimension $ toList dims
                                                     FirstDimension -> head dims
                                          tagOps d = [ ("'l", mkValue d dLength)
                                                   , ("'w", mkValue d dWidth)
                                                   , ("'h", mkValue d dHeight)
                                                   ]
                                          mkValue d f = SetValues [ tshow $ floor $ 100 * f d ]
                                      -- we can't update the dimension directly because it will
                                      -- be overriden by the value of 'l ... set when creating thebox
                                      forM boxes $ flip (updateBoxTags $ tagOps dim) 0
                                      r <- executeStatement ec statement
                                      forM boxes \box ->  updateBoxTags (tagOps $ _boxDim box) box 0
                                      return r
        around $ executeStatement ec statement


    ---------
    SplitShelf selector bselm les wes hes statement -> do
      shelves <- narrowCSelector selector ec >>= getShelves
      boxm <- case bselm of
                Nothing -> return $ Nothing 
                Just sel -> do
                   boxes <- narrowCSelector sel ec >>= getBoxes
                   return $ headMay boxes
      newss <- forM shelves \shelf -> do
           [ls, ws, hs] <-
             zipWithM (\exprs defAcc -> do
                forM exprs \exp -> do
                   let exWithRef = replaceRef <$> exp
                       replaceRef ref = case P.parse (parseRef $ defAcc . sMinD) (unpack ref) ref of
                                             Left err -> error $ "Split shelf parameter invalid: "
                                                      <> show exp
                                                      <> " "
                                                      <> show err
                                             Right v -> v
                   evalExpr (dimForSplit boxm shelf)
                            exWithRef
             )
             [les, wes, hes]
             ds
           splitShelf shelf ls ws hs
      executeStatement ec { ecShelves = ecShelves ec <> InExcluded (Just $ concatMap (map shelfId) newss) Nothing }  statement <* forM newss \(updated:_) ->  unSplitShelf updated

      

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
  narrowFor :: Proxy selector -> ExContext s -> ExContext s -> ExContext s
  -- ^            new context , original
  

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
  narrowFor _ new ec =  ec { ecBoxes = ecBoxes new }
  
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
  narrowFor _ new ec = ec { ecShelves = ecShelves new }

narrowCSelector :: forall selector s . Narrowing selector => CSelector selector -> ExContext s -> WH (ExContext s) s
narrowCSelector cselector ec = do
   let sel = Proxy :: Proxy selector
   ec' <- case cselector of
       CSelector selector -> narrow selector ec
       Parent -> return $ narrowFor sel (fromMaybe withAll (ecParent ec)) ec
       Root -> return $ narrowFor sel withAll ec --  . show $  up $ error $ show ec
       SwapContext -> return $ inverseBoxes ec 
       CStatement stmt -> executeStatement ec stmt
       CUseContext ->  do
          selector :: selector <- useContext ec
          narrow selector ec
       CSelectorAnd s1 s2 -> narrowCSelector s1 ec >>= narrowCSelector s2
   return ec' { ecNoEmptyBoxes = ecNoEmptyBoxes ec, ecNoEmptyShelves = ecNoEmptyShelves ec }

   -- where up e = maybe e up (ecParent e)
