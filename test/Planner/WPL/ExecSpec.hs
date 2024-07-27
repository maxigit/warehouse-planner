{-# LANGUAGE ImplicitParams #-}
module Planner.WPL.ExecSpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Exec
import WarehousePlanner.WPL.Exec
import WarehousePlanner.Type
import WarehousePlanner.Base (emptyWarehouse)
import WarehousePlanner.WPL.ExContext
import Planner.SpecUtil

spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = describe "WPL" do
  let ?shelves = ["S1", "S2", "S3" ]
      ?boxes = ["S1 B-2 B-1#id=1 A-3 A-1 A-2 B-1#id=2"]
  context "sorting" do
     it "sort everything by default *" do
        select "*" `shouldReturn` "A-1 A-2 A-3 B-1#1 B-1#2 B-2"
     it "sort everything by default" do
        select "^^^" `shouldReturn` "A-1 A-2 A-3 B-1#1 B-1#2 B-2"
     it "sort by style (only) by default" do
        select "^=^=^=" `shouldReturn` "A-3 A-1 A-2 B-2 B-1#1 B-1#2"
     it "can be sort by original id" do
      select "^^^={id}" `shouldReturn` "B-2 B-1#1 A-3 A-1 A-2 B-1#2"
     context "after sorting" do
         let base = "^=^=-{style}^={content}"
         --   B-1 B-1 A-1
         --   B-2 A-2
         --   A-3
         it "should be" do
            select base `shouldReturn` "B-1#1 B-1#2 A-1 B-2 A-2 A-3"
         it "doesn't resort the second time" do
            select (base <> " ^^^") `shouldReturn` "B-1#1 B-1#2 A-1 B-2 A-2 A-3"
         it "limits according to the initial sort (global)" do
            -- one per content
            select (base <> " ^^1") `shouldReturn` "B-1#1 B-2 A-3"
         it "limits according to the initial sort (2nd)" do
            -- one per content-style
            select (base <> " ^1") `shouldReturn` "B-1#1 A-1 B-2 A-2 A-3"
         it "limits according to the new sort (2nd)" do
            -- one per content-style
            select (base <> " ^-[id]1") `shouldReturn` "B-1#2 A-1 B-2 A-2 A-3"
         context "allows subsorting" do
            it "with =" do
               select (base <> " ^^={style}^") `shouldReturn` "A-1 B-1#1 B-1#2 A-2 B-2 A-3"
            it "without =" do
               select (base <> " ^^-[id]^") `shouldReturn` "B-1#2 B-1#1 A-1 B-2 A-2 A-3"

     
      
      
-- *  Helper
-- select :: Text -> _WH Text s
select wpl = do
   runWith wpl $ \(Just ec) -> case included $ ecBoxes ec of
                         Nothing -> return "Nothing"
                         Just ids -> do
                             boxes <- mapM (findBox . fst) ids
                             return $ boxesWithId boxes

-- runWith  :: MonadIO (mio r) => Text -> (Maybe (ExContext s) -> WH r s) -> mio r s
runWith wpl action = execWH (emptyWarehouse $ fromGregorian 2024 07 22) do
   shelves <- makeShelves ?shelves
   boxes <- makeBoxes ?boxes
   let Right statements = parseWPL "<source>" wpl
   runWPLWith action statements
   


 
