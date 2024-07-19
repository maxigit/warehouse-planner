{-# Language BlockArguments #-}
{-# Language ImplicitParams #-}
module Planner.SelectorSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Base
import WarehousePlanner.Move
import WarehousePlanner.Exec
import WarehousePlanner.Selector(parseBoxNumberSelector, parseBoxSelector)
import Planner.SpecUtil

spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = describe "Selector" do
   let ?shelves = ["S1", "S2", "S3"]
       ?useDefault = UseDefault
   context "sorting" do
      context "by default" do
              it "sorts by style and content" do
                 let ?boxes = ["S1 B-2 B-1  A-3 A-1 A-2"]
                 limit "" `shouldReturn` "A-1 A-2 A-3 B-1 B-2"
              it "sorts styles by global priory " do
                 let ?boxes = ["S1 B-1#@global=1  A-1#@global=2"]
                 limit "" `shouldReturn` "B-1 A-1"
              it "sort contents by style priory " do
                 let ?boxes = ["S1 A-1#@style=2 A-2#@style=1"]
                 limit "" `shouldReturn` "A-2 A-1" 
              it "sort within content by @content priority " do
                 let ?boxes = ["S1 A-1#@content=2#id=1 A-1#@content=1#id=2"]
                 limit "" `shouldReturn` "A-1#2 A-1#1" 
      context "with tag" do
              let ?boxes = ["S1 A-2#p=2 A-1#p=3 B-1#p=2 B-2#p=1"]
              it "sort before style by given priory " do
                 limit "^^^[p]" `shouldReturn` "B-2 A-2 B-1 A-1" 
              it "sort within style by given priory " do
                 limit "^^[p]" `shouldReturn` "A-2 A-1 B-2 B-1" 
              it "sort by style before given content priory " do
                 limit "^[p]" `shouldReturn` "A-1 A-2 B-1 B-2" 
      context "without sorting" do
          let ?boxes = ["S1 B-2 B-1  A-3 A-1 A-2"]
          it "keep initial order" do
             limit "^=^=^=" `shouldReturn` "B-2 B-1 A-3 A-1 A-2"
   context "limiting" do
      context "by default" do
         let ?boxes = ["S1 A-3 B-1 C-1 B-2 A-1 C-2 A-2"]
         --          A-1 A-2 A-3
         --          B-1 B-2
         --          C-1 C-2
         context "global" do
                 it "takes first sorted by style and content" do
                   limit "^^^1" `shouldReturn` "A-1"
                 it "takes firsts sorted" do
                   limit "^^^4" `shouldReturn` "A-1 A-2 A-3 B-1"
         context "style" do
                 it "takes one of each style sorted by content" do
                   limit "^^1" `shouldReturn` "A-1 B-1 C-1"
                 it "takes two of each style" do
                   limit "^^2" `shouldReturn` "A-1 A-2 B-1 B-2 C-1 C-2"
                 it "and global" do
                   limit "^^2^3" `shouldReturn` "A-1 A-2 B-1" --  B-2"
                 context "explicit" do
                         it "takes one of each style sorted by content" do
                           limit "^^{style}1" `shouldReturn` "A-1 B-1 C-1"
                         it "takes two of each style" do
                           limit "^^{style}2" `shouldReturn` "A-1 A-2 B-1 B-2 C-1 C-2"
                         it "and global" do
                           limit "^^{style}2^3" `shouldReturn` "A-1 A-2 B-1" --  B-2"
                 context "explicit and limiting" do
                    it "sorts all by regardless of style" do
                        limit "^^^{content}" `shouldReturn` "A-1 B-1 C-1 A-2 B-2 C-2 A-3"
                    it "takes 1 of everything ..." do
                        limit "^^1^{content}" `shouldReturn` "A-1 B-1 C-1 A-2 B-2 C-2 A-3"
         context "content" do
                 let ?boxes = ["S1 A-1#id=1 B-1 C-1#id=1 B-2 A-1#id=2 C-1#id=2 A-2"]
                 -- A-1 A-1     A-2
                 -- B-1         B-2
                 -- C-1 C-1
                 it "takes first of each content " do
                   limit "^1" `shouldReturn` "A-1#1 A-2 B-1 B-2 C-1#1"
                 it "takes firsts sorted" do
                   limit "^2" `shouldReturn` "A-1#1 A-1#2 A-2 B-1 B-2 C-1#1 C-1#2"
                 it "with styles" do
                    limit "^1^{style}2" `shouldReturn` "A-1#1 A-2 B-1 B-2 C-1#1"
      context "by shelf" do
          let ?boxes = ["S1 A-1 A-2 B-1"
                       ,"S2 A-3 A-2 B-1 B-2"
                       ,"S3 B-1 C-1"
                       ]
          context "{shelfname}" do
                  it "take one of each shelf ordered by style" do
                     limit "^^={style}1^={shelfname}"  `shouldReturn` "A-1 A-3 B-1"
                  it "take one of each shelf ordered by style/content" do
                     limit "^^{style}1^={shelfname}"  `shouldReturn` "A-1 A-2 B-1"
                  it "take one of each shelf ordered by content regardless of style" do
                     limit "^^1^={shelfname}"  `shouldReturn` "A-1 B-1 B-1"
                  it "take 2 of each shelf" do
                     limit "^^={style}2^={shelfname}"  `shouldReturn` "A-1 A-2 A-3 A-2 B-1 C-1"
   context "with tag" do
         let ?boxes = [ "S1 B-1#id=4"
                      , "S1 A-1#id=2 A-1#id=3 A-1#id=1"
                      , "S1 B-1#id=4 B-2#id=6 B-2#id=5"
                      ]
         context "sort by priority but group by content" do
            it "take hight priority of each content" do
               limit "^[id]1"  `shouldReturn` "A-1#1 B-1#4 B-2#5"
            it "take hight priority of each explicit content" do
               limit "^[id]1^{content}"  `shouldReturn` "A-1#1 B-1#4 B-2#5"
            it "take hight priority of each content" do
               limit "^[id]2"  `shouldReturn` "A-1#1 A-1#2 B-1#4 B-1#4 B-2#5 B-2#6"
            it "take hight priority of each explicit content" do
               limit "^[id]1^^=-{style}"  `shouldReturn` "B-1#4 B-2#5 A-1#1"
   context "without default" do
      let ?useDefault = DontUseDefault
      let ?boxes = [ "S1 B-1#id=4"
                   , "S1 A-1#id=2 A-1#id=3 A-1#id=1"
                   , "S1 B-2#id=6 B-1#id=4 B-2#id=5"
                   ]
      it "doesn't change order with nothing" do
         limit "" `shouldReturn` "B-1#4 A-1#2 A-1#3 A-1#1 B-2#6 B-1#4 B-2#5"
      it "doesn't change order with nothing (^)" do
         limit "^" `shouldReturn` "B-1#4 A-1#2 A-1#3 A-1#1 B-2#6 B-1#4 B-2#5"
      it "doesn't change order with nothing (^^)" do
         limit "^^" `shouldReturn` "B-1#4 A-1#2 A-1#3 A-1#1 B-2#6 B-1#4 B-2#5"
      it "doesn't change order with nothing (^^^)" do
         limit "^^^" `shouldReturn` "B-1#4 A-1#2 A-1#3 A-1#1 B-2#6 B-1#4 B-2#5"
      it "keeps the first one (global)" do
         limit "^^^1" `shouldReturn` "B-1#4"
      it "keeps the first one (content)" do
         -- no group are made, therefore everything should be in one big group
         limit "^1" `shouldReturn` "B-1#4"
      it "keeps original sub order" do
          limit "^^{style}" `shouldReturn` "A-1#2 A-1#3 A-1#1 B-1#4 B-2#6 B-1#4 B-2#5"
      it "behaves not as with UseDefault with '='" do
          without <- limit "^^{style}"
          let ?useDefault = UseDefault
          limit "^^{style}" `shouldNotReturn` without
      it "behaves as with UseDefault with '='" do
          without <- limit "^^{style}"
          let ?useDefault = UseDefault
          limit "^=^={style}^=" `shouldReturn` without


   it "selects" do
      let ?shelves = ["S1", "S2", "S3"]
          ?boxes = ["S1 A-3 A-1 A-2"]
      select "#'1" `shouldReturn` ["A-1"]




-- * Helper
select selection = execWH (emptyWarehouse $ fromGregorian 2024 07 15) do
   shelves <- makeShelves ?shelves
   boxes <- makeBoxes ?boxes
   selected <- findBoxByNameAndShelfNames  (parseBoxSelector selection)
   return $ map boxStyleAndContent selected
   
limit selection = execWH (emptyWarehouse $ fromGregorian 2024 07 15) do
   shelves <- makeShelves ?shelves
   boxes <- makeBoxes ?boxes
   let selected = limitByNumber ?useDefault (parseBoxNumberSelector $ drop 1 selection) boxes
   --                                                                   ^^^^^
   -- parseBoxNumberSelector doesn't expect a ^ at the begining as parseBox
   -- the boxname and the box number selector)
   return $ boxesWithId $ map fst selected
   
   
   

   

