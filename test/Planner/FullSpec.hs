{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Check there is no regression with full scenarios
module Planner.FullSpec where

import WarehousePlanner.Repl
import WarehousePlanner.Report as Report
import WarehousePlanner.Type
import WarehousePlanner.Selector
import WarehousePlanner.Org
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
                 E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01/1#n=01#position== 2:1:2,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01/1#n=02#position== 2:2:2,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01/1#n=03#position== 2:3:2,69.00,42.00,38,|=
                 pending,|2:2:16+0+0+27,M71L07DF-KLB##barcode=DL23NV00277L#done#location=E02.01/3#n=04#position== 1:1:2,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB##barcode=DL23NV00275X#done#location=E02.01/3#n=05#position== 1:2:2,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB##barcode=DL23AP02942M#done#location=E02.01/3#n=06#position== 1:3:2,69.00,42.00,38,=|
                 E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23AP02941F#done#location=E02.01/3#n=07#position== 2:1:1,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23AP02940Y#done#location=E02.01/3#n=08#position== 2:2:1,69.00,42.00,38,=|
                 E02.01/3,=1:3:2,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
                 E02.01/3,=2:1:1,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
                 E02.01/3,=2:2:1,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
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
            E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01/1#n=01#position== 2:1:2,69.00,42.00,38,|=
            E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01/1#n=02#position== 2:2:2,69.00,42.00,38,|=
            E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01/1#n=03#position== 2:3:2,69.00,42.00,38,|=
            E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23NV00277L#location=E02.01/3#n=04#position== 1:1:2,69.00,42.00,38,=|
            E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23NV00275X#location=E02.01/3#n=05#position== 1:2:2,69.00,42.00,38,=|
            E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23AP02942M#location=E02.01/3#n=06#position== 1:3:2,69.00,42.00,38,=|
            E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23AP02941F#location=E02.01/3#n=07#position== 2:1:1,69.00,42.00,38,=|
            E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23AP02940Y#location=E02.01/3#n=08#position== 2:2:1,69.00,42.00,38,=|
            pending,|2:2:16+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
            pending,|2:1:17+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
            pending,|2:2:17+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
            :END:|]
      it "remove unused KLB" do 
         ss <- lExec "klb-unused" $ Report.generateStockTakes (Just $ parseBoxSelector "#'KLB")
         -- same as klb but dead should be removed
         unlines ss `shouldBe` [here|:STOCKTAKE:
                 Bay No,Position,Style,Length,Width,Height,Orientations
                 E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01/1#n=01#position== 2:1:2,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01/1#n=02#position== 2:2:2,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01/1#n=03#position== 2:3:2,69.00,42.00,38,|=
                 pending,|2:2:16+0+0+27,M71L07DF-KLB##barcode=DL23NV00277L#done#location=E02.01/3#n=04#position== 1:1:2,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB##barcode=DL23NV00275X#done#location=E02.01/3#n=05#position== 1:2:2,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB##barcode=DL23AP02942M#done#location=E02.01/3#n=06#position== 1:3:2,69.00,42.00,38,=|
                 E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23AP02941F#done#location=E02.01/3#n=07#position== 2:1:1,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23AP02940Y#done#location=E02.01/3#n=08#position== 2:2:1,69.00,42.00,38,=|
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
            E02.01/2,=1:1:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01/1#n=01#position== 2:1:2,69.00,42.00,38,|=
            E02.01/2,=1:2:2,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01/1#n=02#position== 2:2:2,69.00,42.00,38,|=
            E02.01/2,=1:3:2,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01/1#n=03#position== 2:3:2,69.00,42.00,38,|=
            E02.01/3,=1:1:2,M71L07DF-KLB##barcode=DL23NV00277L#location=E02.01/3#n=04#position== 1:1:2,69.00,42.00,38,=|
            E02.01/3,=1:2:2,M71L07DF-KLB##barcode=DL23NV00275X#location=E02.01/3#n=05#position== 1:2:2,69.00,42.00,38,=|
            E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23AP02942M#location=E02.01/3#n=06#position== 1:3:2,69.00,42.00,38,=|
            E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23AP02941F#location=E02.01/3#n=07#position== 2:1:1,69.00,42.00,38,=|
            E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23AP02940Y#location=E02.01/3#n=08#position== 2:2:1,69.00,42.00,38,=|
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
                 E02.01/2,=1:1:2,M71L07DF-KLB##@content=5#barcode=DL23NV00277L#done#location=E02.01/3#n=04#position== 1:1:2,69.00,42.00,38,|=
                 E02.01/2,=1:2:2,M71L07DF-KLB##@content=5#barcode=DL23NV00275X#done#location=E02.01/3#n=05#position== 1:2:2,69.00,42.00,38,|=
                 E02.01/2,=1:3:2,M71L07DF-KLB##@content=5#barcode=DL23AP02942M#done#location=E02.01/3#n=06#position== 1:3:2,69.00,42.00,38,|=
                 E02.01/3,=1:1:2,M71L07DF-KLB##@content=5#barcode=DL23AP02941F#done#location=E02.01/3#n=07#position== 2:1:1,69.00,42.00,38,=|
                 E02.01/3,=1:2:2,M71L07DF-KLB##@content=5#barcode=DL23AP02940Y#done#location=E02.01/3#n=08#position== 2:2:1,69.00,42.00,38,=|
                 E02.01/3,=1:3:2,M71L07DF-KLB##barcode=DL23NV00276E#done#first#location=E02.01/1#n=01#position== 2:1:2,69.00,42.00,38,=|
                 E02.01/3,=2:1:1,M71L07DF-KLB##barcode=DL23NV00278S#done#location=E02.01/1#n=02#position== 2:2:2,69.00,42.00,38,=|
                 E02.01/3,=2:2:1,M71L07DF-KLB##barcode=DL23NV00279Z#done#location=E02.01/1#n=03#position== 2:3:2,69.00,42.00,38,=|
                 pending,|2:2:16+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
                 pending,|2:1:17+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
                 pending,|2:2:17+0+0+27,M71L07DF-KLB#dead#done#ghost,69.00,42.00,38,=|
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
      rows `shouldBe'` lines expected
   context "export is idempotent" do
     let checkIdem file extras = do 
                   --  expand to full scenario
                   rows <- loadAndExport "data" (file : extras)
                   -- save it
                   withSystemTempDirectory ("whp-spec" <> file) \tmpdir  -> do
                       let path = "export"
                       writeFileUtf8 (tmpdir </> path <.> "org") $ unlines rows
                       rerows <- loadAndExport tmpdir [path]
                       rerows `shouldBe'` rows
     it "pl-65 with check " do
        checkIdem "pl-65" ["check"]
     it "full" do
        checkIdem "full" []
   context "exit modes" do
     context "fill T* as a bay" do
             forM_ [ ("seq", "WPL sequence")
                   , ("cases", "WPL [..]")
                   , ("to", "mixes ^ and >")
                   ] \(path, title) -> 
                  it title do
                   rows <- loadsAndExec "data/ExitMode" ["base",path] (generateStockTakes Nothing)
                   unlines rows `shouldBe` [here|:STOCKTAKE:
                                                 Bay No,Position,Style,Length,Width,Height,Orientations
                                                 S1,|1:1:1,A#id=01,45.00,20.00,10,=|
                                                 S1,|1:1:2,A#id=02,45.00,20.00,10,=|
                                                 S1,|2:1:1,A#id=03,45.00,20.00,10,=|
                                                 S1,|2:1:2,A#id=04,45.00,20.00,10,=|
                                                 S1,|3:1:1,A#id=05,45.00,20.00,10,=|
                                                 S1,|3:1:2,A#id=06,45.00,20.00,10,=|
                                                 T1,|1:1:1,A#id=07,45.00,20.00,10,=|
                                                 T1,|1:1:2,A#id=08,45.00,20.00,10,=|
                                                 T2,|1:1:1,A#id=09,45.00,20.00,10,=|
                                                 T2,|1:1:2,A#id=10,45.00,20.00,10,=|
                                                 T1,|2:1:1,A#id=11,45.00,20.00,10,=|
                                                 T1,|2:1:2,A#id=12,45.00,20.00,10,=|
                                                 T2,|2:1:1,A#id=13,45.00,20.00,10,=|
                                                 T2,|2:1:2,A#id=14,45.00,20.00,10,=|
                                                 T1,|3:1:1,A#id=15,45.00,20.00,10,=|
                                                 T1,|3:1:2,A#id=16,45.00,20.00,10,=|
                                                 T2,|3:1:1,A#id=17,45.00,20.00,10,=|
                                                 T2,|3:1:2,A#id=18,45.00,20.00,10,=|
                                                 U1,|1:1:1,A#id=19,45.00,20.00,10,=|
                                                 U1,|1:1:2,A#id=20,45.00,20.00,10,=|
                                                 U1,|2:1:1,A#id=21,45.00,20.00,10,=|
                                                 U1,|2:1:2,A#id=22,45.00,20.00,10,=|
                                                 U1,|3:1:1,A#id=23,45.00,20.00,10,=|
                                                 U1,|3:1:2,A#id=24,45.00,20.00,10,=|
                                                 :END:|]
     context "fill S* and T* as two bays" do
             forM_ [ ("S-T", "S* and T*")
                   ] \(path, title) -> 
                  it title do
                   rows <- loadsAndExec "data/ExitMode" ["base",path] (generateStockTakes Nothing)
                   unlines rows `shouldBe` [here|:STOCKTAKE:
                                                 Bay No,Position,Style,Length,Width,Height,Orientations
                                                 S1,|1:1:1,A#id=01,45.00,20.00,10,=|
                                                 S1,|1:1:2,A#id=02,45.00,20.00,10,=|
                                                 S2,|1:1:1,A#id=03,45.00,20.00,10,=|
                                                 S2,|1:1:2,A#id=04,45.00,20.00,10,=|
                                                 S1,|2:1:1,A#id=05,45.00,20.00,10,=|
                                                 S1,|2:1:2,A#id=06,45.00,20.00,10,=|
                                                 S2,|2:1:1,A#id=07,45.00,20.00,10,=|
                                                 S2,|2:1:2,A#id=08,45.00,20.00,10,=|
                                                 S1,|3:1:1,A#id=09,45.00,20.00,10,=|
                                                 S1,|3:1:2,A#id=10,45.00,20.00,10,=|
                                                 S2,|3:1:1,A#id=11,45.00,20.00,10,=|
                                                 S2,|3:1:2,A#id=12,45.00,20.00,10,=|
                                                 T1,|1:1:1,A#id=13,45.00,20.00,10,=|
                                                 T1,|1:1:2,A#id=14,45.00,20.00,10,=|
                                                 T2,|1:1:1,A#id=15,45.00,20.00,10,=|
                                                 T2,|1:1:2,A#id=16,45.00,20.00,10,=|
                                                 T1,|2:1:1,A#id=17,45.00,20.00,10,=|
                                                 T1,|2:1:2,A#id=18,45.00,20.00,10,=|
                                                 T2,|2:1:1,A#id=19,45.00,20.00,10,=|
                                                 T2,|2:1:2,A#id=20,45.00,20.00,10,=|
                                                 T1,|3:1:1,A#id=21,45.00,20.00,10,=|
                                                 T1,|3:1:2,A#id=22,45.00,20.00,10,=|
                                                 T2,|3:1:1,A#id=23,45.00,20.00,10,=|
                                                 T2,|3:1:2,A#id=24,45.00,20.00,10,=|
                                                 :END:|]


       
-- |  Load a scenario and compare the exported stocktake (ie boxes and exact position)
-- to a given result.
  
loadAndExec :: FilePath -> FilePath -> WH a RealWorld -> IO a
loadAndExec ipath path action = loadsAndExec ipath [path] action
loadsAndExec :: FilePath -> [FilePath] -> WH a RealWorld -> IO a
loadsAndExec ipath paths action = do
  initRepl ipath
  loads paths
  exec action
  

loadAndExport :: FilePath -> [FilePath] -> IO [Text]
loadAndExport ipath paths = do
  initRepl ipath
  scenario <- loads paths
  let bare = scenario { sInitialState = Nothing
                      , sSteps = filter (\case
                                          (Step h _ _) -> h `elem` [LayoutH, ShelvesH, OrientationsH, ShelfTagsH, ShelfSplitH, ShelfJoinH]
                                          _ -> False
                                        )
                                        $ sSteps scenario
                      , sLayout = sLayout scenario
                      , sColourMap = sColourMap scenario
                      }
  headers <- scenarioToFullText bare
  stocks <- exec $ generateStockTakes Nothing
  return $ lines headers <> stocks





            
  
keepDiff :: Eq a => Int -> [a] -> [a] -> ([a], [a])
keepDiff n xs ys= unzip $ take n $ filter (not . same) $ zip xs ys
  where same = uncurry (==)
  
  
shouldBe' xs ys = uncurry shouldBe $ keepDiff 5 xs ys
  




