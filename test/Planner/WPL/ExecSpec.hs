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
      ?dim = mempty
  context "sorting" do
     it "sort everything by default *" do
        -- by style then by content then creating order
        select "*" `shouldReturn` "A-1 A-2 A-3 B-1#1 B-1#2 B-2"
     it "sort by style only default ^^^" do
        -- mainly because boxes are map by style see boxMap
        select "^^^" `shouldReturn` "A-3 A-1 A-2 B-2 B-1#1 B-1#2"
     it "sort everything by default ^" do
        -- by style then by content then creating order
        select "^" `shouldReturn` "A-1 A-2 A-3 B-1#1 B-1#2 B-2"
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
            it "with =/use previous" do
               select (base <> " ^^={style}^") `shouldReturn` "A-1 B-1#1 B-1#2 A-2 B-2 A-3"
            it "with default " do
               select (base <> " ^^{style}^") `shouldReturn` "A-1 B-1#1 B-1#2 A-2 B-2 A-3"
            it "with @/ use priority default" do
               select (base <> " ^^@-[id]^") `shouldReturn` "B-1#2 B-1#1 A-1 B-2 A-2 A-3"
  context "case to collect" do
    it "collects in order A2 then A3" do
      select (unlines [ "A   | A#'2"
                      , "    | A#'3"
                      ]
             ) `shouldReturn` "A-2 A-3"
    it "collects wihout context" do
      select (unlines [ "| A#'2"
                      , "| A#'3"
                      ]
             ) `shouldReturn` "A-2 A-3"
    it "collects A3 then A2" do
      select (unlines [ "& A   || A#'3"
                      , "      || A#'2"
                      , "& /#"
                      ]
             ) `shouldReturn` "A-3 A-2"
    it "collects using ^ using natural order" do
      select (unlines [ "A      | ^^^1" -- 1 box
                      , "       | ^^^1" -- another one
                      ]
             ) `shouldReturn` "A-1 A-2"
    it "collects using ^ using original order" do
      select (unlines [ "A^^   | ^^^1" -- 1 box
                      , "      | ^^^1" -- another one
                      ]
             ) `shouldReturn` "A-3 A-1"
    it "collects with each case order" do
       select (unlines [ "* | A^^-{content}2"
                       , "  | B^^1"
                       ]
              ) `shouldReturn` "A-3 A-2 B-1#1"
  context "nested  cases" do
    it "" do
      select (unlines [ "* || A | #'2" -- collect A-2
                      , "  || B | #'1" -- and B-1
                      ]
             ) `shouldReturn` "A-2 B-1#1 B-1#2"
    it "" do
      select (unlines [ "* |  & | A#'3" -- collect A-2
                      , "       | B#'1" -- and B-1
                      , "     & #id=1"  -- only B-1#1
                      , "  |  A#'3"     -- add A-3
                      ]
             ) `shouldReturn` "B-1#1 A-3"
  context "ranges" do
     let ?boxes = ["S1 B-2 B-1#id=1 A-3 A-1 A-2 B-1#id=2"]
     it "from" do
       select "^^^={id} from A" `shouldReturn` "A-3 A-1 A-2 B-1#2"
     it "before" do
       select "^^^={id} before A" `shouldReturn` "B-2 B-1#1"
     it "upto" do
       select "^^^={id} upto A" `shouldReturn` "B-2 B-1#1 A-3 A-1 A-2"
     it "upto first" do
       select "^^^={id} upto A^^^1" `shouldReturn` "B-2 B-1#1 A-3"
     it "after" do
       select "^^^={id} after A" `shouldReturn` "B-1#2"
     context "null boundaries" do
             let all = "B-2 B-1#1 A-3 A-1 A-2 B-1#2"
             it "from nothing -> nothing" do
               select "^^^={id} from X" `shouldReturn` ""
             it "before nothing -> all" do
               select "^^^={id} before X" `shouldReturn` all
             it "upto nothing -> all " do
               select "^^^={id} upto X" `shouldReturn` all
             it "after nothing -> nothing" do
               select "^^^={id} after X" `shouldReturn` ""
  context "tagFor" do
    let ?boxes = ["S1 B-2 B-1#id=1 A-3 A-1 A-2 B-1#id=2"]
    it "tags temporarily" do
      -- tag with tempTag temporary
      -- uses this tag to do something
      -- at the end the tempTag should have disappeared
      select ( unlines [ "tag:for A tempTag"
                       , "   #tempTag^^^1 tag theTag "
                       , "#theTag"
                       ]
             ) `shouldReturn` "A-3"
    it "remove the temporary tags" do
      select ( unlines [ "tag:for A tempTag"
                       , "   #tempTag^^^1 tag theTag "
                       , "#tempTag"
                       ]
             ) `shouldReturn` ""
       


       

     
      
      
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
   let statements = case parseWPL "<source>" wpl of
                         Right st -> st
                         Left err -> error $ unpack err
   runWPLWith action statements
   


 
