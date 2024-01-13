{-# LANGUAGE ImplicitParams #-}
-- | Run Warehouse in IO
module WarehousePlanner.Repl
( initRepl
, load
, loads
, summary
-- summary
) where 
import ClassyPrelude hiding(init)
import WarehousePlanner.Base
import WarehousePlanner.Org
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
          writeIORef stateRef state { warehouse = wh }
          print wh

summary :: IO ()
summary = do
  state <- readIORef stateRef
  r@(tables, extra) <- execWH (warehouse state) Report.summary
  mapM_ print tables
  print "------------"
  print extra
  -- mapM_ print extra


  

  




