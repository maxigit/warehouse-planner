-- | Check there is no regression with full scenarios
module Planner.FullSpec where

import WarehousePlanner.Repl
import WarehousePlanner.Report
import Test.Hspec
import ClassyPrelude

spec :: Spec
spec = describe "Full scenario" do
   it "read full-with-moves" do
      checkFull "full-with-moves"
       
       
-- |  Load a scenario and compare the exported stocktake (ie boxes and exact position)
-- to a given result.
checkFull path = do
  let dir = "data"
  initRepl dir
  load path
  rows <- exec (generateStockTakes Nothing)
  expected <- readFileUtf8 (dir </> path <.> "stocktake")
  writeFileUtf8 "result.org" $ unlines rows
  rows `shouldBe'` lines expected
  
  
keepDiff :: Eq a => Int -> [a] -> [a] -> ([a], [a])
keepDiff n xs ys= unzip $ take n $ filter (not . same) $ zip xs ys
  where same = uncurry (==)
  
  
shouldBe' xs ys = uncurry shouldBe $ keepDiff 5 xs ys
  




