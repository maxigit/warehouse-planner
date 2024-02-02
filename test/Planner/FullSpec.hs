{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Check there is no regression with full scenarios
module Planner.FullSpec where

import WarehousePlanner.Repl
import WarehousePlanner.Report as Report
import WarehousePlanner.Type
import Test.Hspec
import ClassyPrelude

spec :: Spec
spec = describe "Full scenario" do
   let execFull = loadAndExec dir path
       dir = "data"
       path = "full-with-moves"
   it "reads full-with-moves" do
      rows <- execFull (generateStockTakes Nothing)
      expected <- readFileUtf8 (dir </> path <.> "stocktake")
      -- writeFileUtf8 "result.org" $ unlines rows
      rows `shouldBe'` lines expected
   it "summarizes it " do
      sums <- execFull Report.summary
      sums `shouldBe` 
             ( [ [""        , "used", "total", "free", "%used", "floor"]
               , ["<main>"  , "299" , "801"  , "503" , "37"   , "662"]
               , ["_error"  , "60"  , ""     , ""    , ""     , ""   ]
               , ["_pending", "370" , ""     , ""    , ""     , ""   ]
               , ["top"     , "53"  , "353"  , "300" , "15"   , "224"]
               ]
             , [              "782" , "1155" , "373" , "68"   , "886"]
             )
       
-- |  Load a scenario and compare the exported stocktake (ie boxes and exact position)
-- to a given result.
  
loadAndExec :: FilePath -> FilePath -> WH a RealWorld -> IO a
loadAndExec ipath path action = do
  initRepl ipath
  load path
  exec action
  

  
keepDiff :: Eq a => Int -> [a] -> [a] -> ([a], [a])
keepDiff n xs ys= unzip $ take n $ filter (not . same) $ zip xs ys
  where same = uncurry (==)
  
  
shouldBe' xs ys = uncurry shouldBe $ keepDiff 5 xs ys
  




