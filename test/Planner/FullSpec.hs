{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Check there is no regression with full scenarios
module Planner.FullSpec where

import WarehousePlanner.Repl
import WarehousePlanner.Report as Report
import WarehousePlanner.Type
import WarehousePlanner.Selector
import Test.Hspec
import ClassyPrelude
import Here


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
   context "rearrange" do
      let dir = "data/Rearrange"
          path = "full"
          lExec p action = initRepl dir >> loads [path, p] >> exec action
      it "swift KLB one by one " do 
         ss <- lExec "klb" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- original situation
         -- 1 2 3 -> E02.01/2
         -- 4 5 6 7 -> E0.01/3 (top)  
         --
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- 1 2 3
         -- =========
         -- 
         -- After moving to pending on Deads
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- C C C                Pending: 1 2 3
         -- =========
         -- Deads go at the end (i.e on top) and top to Pending
         -- =========
         -- 7 8 C C C
         -- ---------
         -- 1 2 3                Pending: 4 5 6
         -- =========
         unlines ss `shouldBe` [here|:STOCKTAKE:
                 Bay No,Position,Style,Length,Width,Height,Orientations
                 E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01'1#n=01#position== 2:1:2#box_id=562,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01'1#n=02#position== 2:2:2#box_id=575,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01'1#n=03#position== 2:3:2#box_id=576,69.00,42.00,38,|=
                 pending,|2:2:16+0+0+27,M71L07DF-KLB##barcode=DL23NV00277L#done#location=E02.01'3#n=04#position== 1:1:2#box_id=582,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB##barcode=DL23NV00275X#done#location=E02.01'3#n=05#position== 1:2:2#box_id=609,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB##barcode=DL23AP02942M#done#location=E02.01'3#n=06#position== 1:3:2#box_id=782,69.00,42.00,38,=|
                 E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23AP02941F#done#location=E02.01'3#n=07#position== 2:1:1#box_id=785,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23AP02940Y#done#location=E02.01'3#n=08#position== 2:2:1#box_id=786,69.00,42.00,38,=|
                 E02.01/3,=1:3:2,M71L07DF-KLB#dead#done#ghost#box_id=894,69.00,42.00,38,=|
                 E02.01/3,=2:1:1,M71L07DF-KLB#dead#done#ghost#box_id=895,69.00,42.00,38,=|
                 E02.01/3,=2:2:1,M71L07DF-KLB#dead#done#ghost#box_id=896,69.00,42.00,38,=|
            :END:|]
      it "keep KLB in place" do 
         ss <- lExec "klb-inplace" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- This time, everything which is not a Dead should stay in place if possible (i.e 4 5)
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- 1 2 3                Pending: C C C
         -- =========
         unlines ss `shouldBe` [here|:STOCKTAKE:
            Bay No,Position,Style,Length,Width,Height,Orientations
            E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01'1#n=01#position== 2:1:2#box_id=562,69.00,42.00,38,|=
            E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01'1#n=02#position== 2:2:2#box_id=575,69.00,42.00,38,|=
            E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01'1#n=03#position== 2:3:2#box_id=576,69.00,42.00,38,|=
            E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23NV00277L#location=E02.01'3#n=04#position== 1:1:2#box_id=582,69.00,42.00,38,=|
            E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23NV00275X#location=E02.01'3#n=05#position== 1:2:2#box_id=609,69.00,42.00,38,=|
            E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23AP02942M#location=E02.01'3#n=06#position== 1:3:2#box_id=782,69.00,42.00,38,=|
            E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23AP02941F#location=E02.01'3#n=07#position== 2:1:1#box_id=785,69.00,42.00,38,=|
            E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23AP02940Y#location=E02.01'3#n=08#position== 2:2:1#box_id=786,69.00,42.00,38,=|
            pending,|2:2:16+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=894,69.00,42.00,38,=|
            pending,|2:1:17+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=895,69.00,42.00,38,=|
            pending,|2:2:17+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=896,69.00,42.00,38,=|
            :END:|]
      it "remove unused KLB" do 
         ss <- lExec "klb-unused" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- same as klb but dead should be removed
         unlines ss `shouldBe` [here|:STOCKTAKE:
                 Bay No,Position,Style,Length,Width,Height,Orientations
                 E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01'1#n=01#position== 2:1:2#box_id=562,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01'1#n=02#position== 2:2:2#box_id=575,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01'1#n=03#position== 2:3:2#box_id=576,69.00,42.00,38,|=
                 pending,|2:2:16+0+0+27,M71L07DF-KLB##barcode=DL23NV00277L#done#location=E02.01'3#n=04#position== 1:1:2#box_id=582,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB##barcode=DL23NV00275X#done#location=E02.01'3#n=05#position== 1:2:2#box_id=609,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB##barcode=DL23AP02942M#done#location=E02.01'3#n=06#position== 1:3:2#box_id=782,69.00,42.00,38,=|
                 E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23AP02941F#done#location=E02.01'3#n=07#position== 2:1:1#box_id=785,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23AP02940Y#done#location=E02.01'3#n=08#position== 2:2:1#box_id=786,69.00,42.00,38,=|
            :END:|]
      it "keep KLB in place and remove unused" do 
         ss <- lExec "klb-inplace-unused" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- This time, everything which is not a Dead should stay in place if possible (i.e 4 5)
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- 1 2 3                Pending: C C C
         -- =========
         unlines ss `shouldBe` [here|:STOCKTAKE:
            Bay No,Position,Style,Length,Width,Height,Orientations
            E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01'1#n=01#position== 2:1:2#box_id=562,69.00,42.00,38,|=
            E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01'1#n=02#position== 2:2:2#box_id=575,69.00,42.00,38,|=
            E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01'1#n=03#position== 2:3:2#box_id=576,69.00,42.00,38,|=
            E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23NV00277L#location=E02.01'3#n=04#position== 1:1:2#box_id=582,69.00,42.00,38,=|
            E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23NV00275X#location=E02.01'3#n=05#position== 1:2:2#box_id=609,69.00,42.00,38,=|
            E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23AP02942M#location=E02.01'3#n=06#position== 1:3:2#box_id=782,69.00,42.00,38,=|
            E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23AP02941F#location=E02.01'3#n=07#position== 2:1:1#box_id=785,69.00,42.00,38,=|
            E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23AP02940Y#location=E02.01'3#n=08#position== 2:2:1#box_id=786,69.00,42.00,38,=|
            :END:|]
      -- xit "move top first by setting priority" -- do return ()
      it "swift KLB one by one following priority " do 
         ss <- lExec "klb-top" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- original situation
         -- 1 2 3 -> E02.01/2
         -- 4 5 6 7 -> E0.01/3 (top)  
         --
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- 1 2 3
         -- =========
         -- 
         -- After moving to pending on deads
         -- =========
         -- 4 5 6 7 8
         -- ---------
         -- C C C                Pending: 1 2 3
         -- =========
         -- Tops move to ghosts first, so ghost, top     ,pending
         --                            => top  , pending ,ghost
         -- i.e ghosts are moving to pending and pending to top
         -- =========
         -- 7 8 C C C
         -- ---------
         -- 1 2 3                Pending: 4 5 6
         -- =========
         unlines ss `shouldBe` [here|:STOCKTAKE:
                 Bay No,Position,Style,Length,Width,Height,Orientations
                 E02.01/2,=1:1:2,M71L07DF-KLB##@content=5#barcode=DL23NV00277L#done#location=E02.01'3#n=04#position== 1:1:2#box_id=582,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##@content=5#barcode=DL23NV00275X#done#location=E02.01'3#n=05#position== 1:2:2#box_id=609,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##@content=5#barcode=DL23AP02942M#done#location=E02.01'3#n=06#position== 1:3:2#box_id=782,69.00,42.00,38,|=
                 E02.01/3,=1:1:2,M71L07DF-KLB##@content=5#barcode=DL23AP02941F#done#location=E02.01'3#n=07#position== 2:1:1#box_id=785,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##@content=5#barcode=DL23AP02940Y#done#location=E02.01'3#n=08#position== 2:2:1#box_id=786,69.00,42.00,38,=|
                 E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01'1#n=01#position== 2:1:2#box_id=562,69.00,42.00,38,=|
                 E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01'1#n=02#position== 2:2:2#box_id=575,69.00,42.00,38,=|
                 E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01'1#n=03#position== 2:3:2#box_id=576,69.00,42.00,38,=|
                 pending,|2:2:16+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=894,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=895,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB#dead#done#ghost#box_id=896,69.00,42.00,38,=|
            :END:|]
   it "read pl-65" do
      -- full example with slots, rearrange and sorting
      rows <- loadAndExec "data" "pl-65" (generateStockTakes Nothing)
      expected <- readFileUtf8 ("data" </> "pl-65" <.> "stocktake")
      rows `shouldBe'` lines expected
   it "reads C1 with WPL" do
      -- full example with WPL
      rows <- loadAndExec "data"  "c1" (generateStockTakes Nothing)
      expected <- readFileUtf8 ("data" </> "c1" <.> "stocktake")
      rows `shouldBe` lines expected

       
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
  




