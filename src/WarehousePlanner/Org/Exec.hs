{-# LANGUAGE ImplicitParams #-}
module WarehousePlanner.Org.Exec 
( renderScenario
, renderReport
, execWithCache
, execScenario
, noCache, refCache
, CacheFn(..)
, cacheScenarioIn
, cacheScenarioOut
)where

import ClassyPrelude
import WarehousePlanner.Org.Types
import WarehousePlanner.Org.Internal
import WarehousePlanner.Styling
import WarehousePlanner.Base
import WarehousePlanner.Csv
import WarehousePlanner.Display
-- import Util.Cache
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.State (put)
import WarehousePlanner.Exec
import System.IO.Unsafe (unsafePerformIO)
import Data.Map qualified as Map

-- * Type 
-- | Typeable version of Warehouse. Needed to be cached.

instance Show (Warehouse s) where
  show wh = "Warehouse : " ++ "\n\tBOXES: " ++ show (length $ boxes wh)
                           ++ "\n\tSHELVES: " ++ show (length $ shelves wh)
                           -- ++ "\nGROUPS\n" ++ show (length $ shelfGroup wh)
data WarehouseCache = WarehouseCache (Warehouse ()) deriving (Show, Typeable)
-- * Caching 

data CacheFn m = CacheFn { cache :: forall a k . (Show k, Typeable a) => k -> m a -> m a 
                         , cacheOut :: forall a k . (Show k, Typeable a) => k -> m a -> m a
                         , purgeKey :: forall k . (Show k) => k -> m ()
                         }
               
noCache :: Monad m => CacheFn m
noCache = CacheFn {..} where
  cache _ = id
  cacheOut _ = id
  purgeKey _= return ()


{-# NOINLINE cacheRef #-}
cacheRef :: IORef (Map String a)
cacheRef = unsafePerformIO $ newIORef mempty
refCache :: (MonadIO m, Monad m) => CacheFn m
refCache = CacheFn {..} where
  cache k vm = do 
        v <- vm 
        liftIO $ modifyIORef cacheRef $ Map.insert (makeKey k) v
        return v
  cacheOut k vm = do
           cachedm <- liftIO $ fmap (lookup (makeKey k)) (readIORef cacheRef)
           case cachedm of
                Nothing -> vm
                Just c -> return c
  purgeKey k = liftIO $ modifyIORef cacheRef $ Map.delete (makeKey k)
  makeKey k = show k

cacheWarehouseIn :: (?cache :: CacheFn io, MonadIO io) => DocumentHash -> Warehouse s -> io (Maybe WarehouseCache)
cacheWarehouseIn (DocumentHash key) warehouse = do
  (cache ?cache) ("warehouse", key) (return . Just $ freeze warehouse)

cacheWarehouseOut :: (?cache :: CacheFn io, MonadIO io) => DocumentHash -> io (Maybe WarehouseCache)
cacheWarehouseOut (DocumentHash key) = do
  wcache <- (cacheOut ?cache) ("warehouse", key) (return Nothing)
  -- This hack creates a key if the warehouse doesn't exists
  case wcache of
    Nothing -> do
      (purgeKey  ?cache) ("warehouse", key) -- can occur in concurrency probleme FIXME
      return ()
    _ -> return ()
  return $ wcache

freeze :: Warehouse s -> WarehouseCache
freeze = WarehouseCache . unsafeCoerce

unfreeze :: WarehouseCache -> Warehouse s
unfreeze (WarehouseCache warehouse)= unsafeCoerce warehouse

cacheScenarioIn :: (?cache :: CacheFn io, MonadIO io) =>  Scenario -> io (Text, Int)
cacheScenarioIn sc = do
  let (DocumentHash key) = scenarioKey sc
  layoutSize <- scenarioLayoutSize sc
  (cache ?cache) ("scenario", key) (return $ Just (sc, layoutSize))
  return (key, layoutSize)

cacheScenarioOut :: (?cache :: CacheFn io, MonadIO io) =>  Text -> io (Maybe (Scenario, Int))
cacheScenarioOut key = do
  (cache ?cache) ("scenario", key) (return Nothing)

-- * Exec 

-- | Execute a scenario, read and write cache if necessary.
-- The execution of each steps is cached, so that
-- when modifying a file in the middle results, all the steps
-- at the beginning which haven't changed don't need to be recalculated.
-- To do so, we create and execute a chain of scenario with one step
-- and the initial step corresponding to the previous one.
execScenario :: forall io . (?today :: Day, ?cache :: CacheFn io, MonadIO io) =>  Scenario -> io (Warehouse RealWorld)
execScenario sc@Scenario{..} = do
  initialM <- join <$> cacheWarehouseOut `mapM` sInitialState
  let warehouse0 = maybe (emptyWarehouse ?today) unfreeze initialM
      go :: MonadIO io =>  io (Warehouse RealWorld) -> [Step] -> [Step] -> io (Warehouse RealWorld)
      go w _ [] = w
      go warehouseM (previous) (steps0) = do
        -- in order to only save at saving points
        -- we need to execute all steps between saving points
        -- as a group
        let (toExecutes, steps') = break (== SavingPoint) steps0
            steps = drop 1 steps' -- drop SavingPoint if step non empty
            allPreviousSteps = previous <> toExecutes
        let subKey = warehouseScenarioKey $ Scenario Nothing allPreviousSteps  Nothing mempty
        wM <- cacheWarehouseOut subKey
        let w  = case wM of
                      Nothing -> do
                        warehouse <- warehouseM
                        (wCopyM,_) <- runWH warehouse copyWarehouse
                        execM <- liftIO $ mapM executeStep toExecutes
                        (_, w') <- runWH (emptyWarehouse ?today)  $ do
                          wCopy <- wCopyM
                          -- probably useless now 
                          put wCopy -- { boxStyling = stylingFromTags colourMap, shelfStyling = shelfStylingFromTags colourMap }
                          sequence_ execM
                        cacheWarehouseIn subKey w'
                        return w'
                      Just w' -> {-traceShowM ("Scenario Step => use cache", subKey) >>-} (return $ unfreeze w')
        -- carry on with the remaing steps
        go (w) (allPreviousSteps) steps
  --  update layout if exist
  contentPath <- contentPathM
  let setLayout wh = case sLayout of
                 Nothing -> return wh
                 Just layout -> do
                    groupW <-  liftIO $ readWarehouse (contentPath layout)
                    groups <- execWH wh groupW
                    return $ wh { shelfGroup = groups }
  let setStylings wh = do
        colourMaps <- liftIO $ mapM (readColourMap . contentPath) sColourMap
        let boxStyling = stylingFromTags colourMap
            shelfStyling = shelfStylingFromTags colourMap
            colourMap = concat colourMaps
        return wh { shelfStyling, boxStyling}
  go (return warehouse0) [] (sSortedSteps sc) >>= setLayout >>=  setStylings
  

execWithCache :: (?today :: Day, ?cache :: CacheFn io, MonadIO io) =>  Scenario -> io (Warehouse RealWorld)
-- execWithCache = execScenario
execWithCache sc = do
  let key = warehouseScenarioKey sc
  wM <- cacheWarehouseOut key
  case wM of
    Nothing -> execScenario sc
    Just wh -> return  $ unfreeze wh


renderScenario :: (?today :: Day, ?cache :: CacheFn io, MonadIO io) => Scenario -> Maybe DocumentHash
               -> io (Either String [_Diagram])
renderScenario sc layoutM = do
  contentPath <- contentPathM
  case layoutM <|> (sLayout sc) of
    Nothing -> return $ Left "No layout provided"
    Just layout -> do
        wh0 <- execWithCache sc
        groupW <- liftIO $ readWarehouse (contentPath layout)
        colourMaps <- liftIO $ mapM (readColourMap . contentPath) (sColourMap sc)
        let boxStyling = stylingFromTags colourMap
            shelfStyling = shelfStylingFromTags colourMap
            colourMap = concat colourMaps
        diags <- execWH wh0 ( do
                                group <- groupW
                                mapM (renderRun shelfStyling boxStyling) group
                           )
        return (Right $ toList diags)

renderReport sc report = do
  wh0 <- execWithCache sc
  execWH wh0 report



