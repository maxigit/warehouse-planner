{-# LANGUAGE ImplicitParams #-}
-- | Run Warehouse in IO
module WarehousePlanner.Repl
( initRepl
, load
, loads
, summary
, exec
) where 
import ClassyPrelude hiding(init)
import WarehousePlanner.Base
import WarehousePlanner.Org
import WarehousePlanner.Csv (readWarehouse)
import System.IO.Unsafe
import qualified WarehousePlanner.Report as Report


data ReplState =
  ReplState { currentDir :: FilePath
            , warehouse :: Warehouse RealWorld
            }
            
{-# NOINLINE stateRef #-}
stateRef = unsafePerformIO $ newIORef (error "StateRef not initialized. Used init function")

initRepl :: FilePath -> IO ()
initRepl filepath = do
  now <-  getCurrentTime

  writeIORef stateRef (ReplState filepath
                                 (emptyWarehouse $ utctDay now)
                      )
  
  
load :: FilePath -> IO ()
load path = loads [path]

loads :: [FilePath] -> IO ()
loads paths = do
  state <- readIORef stateRef
  let ?cache = noCache
      ?today = whDay (warehouse state)
  scenarioEs <- forM paths \path -> readScenarioFromPath (importDispatchDef (currentDir state))
                                         (currentDir state </> path <.> "org")
  let scenarioE = sequence scenarioEs
  print scenarioE
  case scenarioE of
    Left e -> error $ unpack e
    Right scenarios -> do
          let scenario = mconcat scenarios
          contentPath <- contentPathM
          wh <- execScenario scenario
          groups <- case sLayout scenario of
                      Nothing -> return $ shelfGroup wh
                      Just layout -> do
                           gw <- readWarehouse (contentPath layout)
                           execWH wh gw
          writeIORef stateRef state { warehouse = wh { shelfGroup = groups}
                                    }
          print wh

summary :: IO ()
summary = do
  state <- readIORef stateRef
  r@(tables, extra) <- execWH (warehouse state) Report.summary
  mapM_ print tables
  print "------------"
  print extra
  -- mapM_ print extra


exec :: WH a RealWorld -> IO a
exec wh = do
  state <- readIORef stateRef
  r <- execWH (warehouse state) wh
  return r
  

  




