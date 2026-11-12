module WarehousePlanner.WPL.ShelfAction 
( runShelfActions
) where

import ClassyPrelude
import WarehousePlanner.Base
import WarehousePlanner.WPL.Types
import WarehousePlanner.WPL.Parser (wplParser)
import WarehousePlanner.Selector (parseBoxSelector, parseShelfSelector, printBoxSelector, parseBoxNumberSelector)
import Text.Megaparsec qualified as P
import Data.Map qualified as Map
import Control.Monad.State (gets)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.List(nub)

runShelfActions :: (ShelfAction Statement -> WH () s) -> ShelfSelector -> WH () s
runShelfActions runAction sel = do
   actionTexts <- getShelfActions
   case partitionEithers (map parseAction actionTexts) of
       ([], actions) -> do
          (sources, _others) <- partitionActions sel actions
          mapM_ (runShelfAction runAction actions) sources
       (errs,_) -> error . unpack $ unlines errs
          
runShelfAction :: (ShelfAction a -> WH () s) -> [ShelfAction a] -> ShelfAction a -> WH () s
runShelfAction runAction others source = do
   -- check if done
   shelves <- getShelfActionShelves source
   let done = all isDone shelves
       isDone shelf = tagIsPresent shelf "@action-done"

   when (any (flip tagIsPresent "@action-running") shelves) do
      error $ "LOOP detected when running action for " <> show (saShelves source)
   unless done $ do 
      -- marks has running first to avoid loop
      forM_ shelves $ void . updateShelfTags (fromTag'Operations $ [("@action-running", SetTag)])
      -- exec dependencies first
      (depss, _) <- unzip <$> mapM (\dep -> partitionActions dep others) (saDependencies source)
      let deps = concat depss
      mapM (runShelfAction runAction others) deps
      
      -- empty content
      s0 <- defaultShelf
      boxes <- filter (not . flip tagIsPresent "@action-locked") . concat <$> mapM findBoxByShelf shelves
      void $ mapM (assignShelf $ Just s0) boxes
      forM_ shelves $ void . updateShelfTags (fromTag'Operations $ [ ("@action-done", SetTag)
                                                                   , ("@action-running", RemoveTag)
                                                                   , ("@action-locked", SetTag)
                                                                   , ("@action-used-deps", SetValues $ map unShelfname
                                                                                                     $ concatMap (toList . saShelves) deps
                                                                     )
                                                                   ]
                                             )

      runAction source
      
  

getShelfActions :: WH [ShelfAction Text] s
getShelfActions = do
  ids <- gets shelves
  shelves <- mapM findShelf ids
  let actions = mapMaybe actionFromShelf $ toList shelves
      -- group actions 
      actionBy = Map.fromListWith mergeAction [ (key a, a) 
                                              | a <- actions
                                              ]
  return  $ toList actionBy
  where mergeAction a1 a2 = ShelfAction (saAction a1) -- ^ we know a1 and a2 have the same action because we are grouping them by action
                                        (saShelves a1 <> saShelves a2)
                                        (saDependencies a1 <> saDependencies a2)
                                        (saBoxes a1)
                                        (headMay $ catMaybes [ saAutoBreak a1, saAutoBreak a2 ]) -- get first autBreak
                                        (saOrder a1) -- ^ part of the key
        key ShelfAction{..} = (saAction, printBoxSelector <$> saBoxes, saOrder )

  
actionFromShelf :: Shelf s -> Maybe (ShelfAction Text)
actionFromShelf shelf = let
  actionTextM = getTagValuem shelf "@action"
  shelfDeps = getTagValues shelf "@action-deps" -- shelf selector 
  fullDeps = getTagValues shelf "@action-deps?" -- can find shelves by box but need "/" for location
  deps = parseShelfSelector <$> fullDeps <> map (cons '/') shelfDeps
  boxes = map parseBoxSelector $ nub $ sort $ getTagValues shelf "@pick" <> getTagValues shelf "@stock"
  break = case getTagValuem shelf "@start" of
           Just "new-slot" -> Just StartNewSlot
           Just "new-slice" -> Just StartNewSlice
           Just "new-shelf" -> Just StartNewShelf
           Just b -> error $ show b <> " is not a valid break usi (slot,slice,shelf) for shelf " <> show (shelfName shelf)
           Nothing -> Nothing
  order = parseBoxNumberSelector True . dropHat  <$> getTagValuem shelf "@order"
  --                                    ^^^^^^
  --                                      |
  --                                      +-- parseBoxNumberSelector doesn't expect a starting ^
  --                                          this is different from a normal selector
  --                                          so we remove a leading ^ to make it compatible with selectors
  dropHat t = case uncons t of 
                     Just ('^', number ) -> number
                     _ -> t

  in case actionTextM of 
     Nothing -> Nothing
     Just "" -> Nothing
     Just action -> Just $ ShelfAction { saAction = action
                                       , saShelves = pure (Shelfname $ shelfName shelf)
                                       , saDependencies = deps
                                       , saBoxes = boxes
                                       , saAutoBreak = break
                                       , saOrder = order
                                       }
  
  
parseAction :: ShelfAction Text -> Either String (ShelfAction Statement)  
parseAction sa = case P.runParser wplParser (show $ saShelves sa) (saAction sa) of 
                  Left bundle -> Left $ P.errorBundlePretty bundle
                  Right [] -> Left $ "empty actions for : " <> show (saShelves sa)
                  Right (a:as) -> Right ShelfAction { saAction = Ors (a :| as)
                                                    , saShelves = saShelves sa
                                                    , saDependencies = saDependencies sa
                                                    , saBoxes = saBoxes sa
                                                    , saAutoBreak = saAutoBreak sa
                                                    , saOrder = saOrder sa
                                                    }
  
partitionActions :: ShelfSelector -> [ShelfAction a] -> WH ([ShelfAction a], [ShelfAction a]) s
partitionActions sel actions = do
   fromSelectorI <- findShelvesByBoxNameAndNames sel
   fromSelector <- mapM findShelf fromSelectorI
   let nameSet = setFromList $ map (Shelfname . shelfName) fromSelector :: Set Shelfname
   let keep s = any (`elem` nameSet) (saShelves s)
   return $ partition keep actions
   
  
   
getShelfActionShelves :: ShelfAction a -> WH (NonEmpty (Shelf s)) s
getShelfActionShelves sa = do
  shelves <- traverse findShelfByName (saShelves sa)
  case nonEmpty $ catMaybes $ toList shelves of 
       Nothing -> error $ "No shelves found for " <> show (saShelves sa) -- bizarre since per construction action should have a valid shelf
       Just ne -> return ne



