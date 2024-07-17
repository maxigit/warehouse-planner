{-# Language BlockArguments #-}
{-# Language ImplicitParams #-}
module Planner.SelectorSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Base
import WarehousePlanner.Move
import WarehousePlanner.Exec
import WarehousePlanner.Selector(parseSelector, parseBoxNumberSelector, parseBoxSelector)
import WarehousePlanner.Report (boxStyleWithTags)
import Data.Text (breakOn)

spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = describe "Selector" do
   let ?shelves = ["S1", "S2", "S3"]
   context "sorting" do
      context "by default" do
              it "sorts by style and content" do
                 let ?boxes = ["S1 B-2 B-1  A-3 A-1 A-2"]
                 limit "" `shouldReturn` ["A-1", "A-2", "A-3", "B-1", "B-2"]
              it "sorts styles by global priory " do
                 let ?boxes = ["S1 B-1#@global=1  A-1#@global=2"]
                 limit "" `shouldReturn` words "B-1 A-1"
              it "sort contents by style priory " do
                 let ?boxes = ["S1 A-1#@style=2 A-2#@style=1"]
                 limit "" `shouldReturn` words "A-2 A-1" 
              it "sort within content by @content priority " do
                 let ?boxes = ["S1 A-1#@content=2#id=1 A-1#@content=1#id=2"]
                 limit "" `shouldReturn` words "A-1#2 A-1#1" 
      context "with tag" do
              let ?boxes = ["S1 A-2#p=2 A-1#p=3 B-1#p=2 B-2#p=1"]
              it "sort before style by given priory " do
                 limit "^^^[p]" `shouldReturn` words "B-2 A-2 B-1 A-1" 
              it "sort within style by given priory " do
                 limit "^^[p]" `shouldReturn` words "A-2 A-1 B-2 B-1" 
              it "sort by style before given content priory " do
                 limit "^[p]" `shouldReturn` words "A-1 A-2 B-1 B-2" 
   context "limiting" do
      context "by default" do
         let ?boxes = ["S1 A-3 B-1 C-1 B-2 A-1 C-2 A-2"]
         --          A-1 A-2 A-3
         --          B-1 B-2
         --          C-1 C-2
         context "global" do
                 it "takes first sorted by style and content" do
                   limit "^^^1" `shouldReturn` words "A-1"
                 it "takes firsts sorted" do
                   limit "^^^4" `shouldReturn` words "A-1 A-2 A-3 B-1"
         context "style" do
                 it "takes one of each style sorted by content" do
                   limit "^^1" `shouldReturn` words "A-1 B-1 C-1"
                 it "takes two of each style" do
                   limit "^^2" `shouldReturn` words "A-1 A-2 B-1 B-2 C-1 C-2"
                 it "and global" do
                   limit "^^2^3" `shouldReturn` words "A-1 A-2 B-1" --  B-2"
                 context "explicit" do
                         it "takes one of each style sorted by content" do
                           limit "^^{style}1" `shouldReturn` words "A-1 B-1 C-1"
                         it "takes two of each style" do
                           limit "^^{style}2" `shouldReturn` words "A-1 A-2 B-1 B-2 C-1 C-2"
                         it "and global" do
                           limit "^^{style}2^3" `shouldReturn` words "A-1 A-2 B-1" --  B-2"
                 context "explicit and limiting" do
                    it "sorts all by regardless of style" do
                        limit "^^^{content}" `shouldReturn` words "A-1 B-1 C-1 A-2 B-2 C-2 A-3"
                    it "takes 1 of everything ..." do
                        limit "^^1^{content}" `shouldReturn` words "A-1 B-1 C-1 A-2 B-2 C-2 A-3"
         context "content" do
                 let ?boxes = ["S1 A-1#id=1 B-1 C-1#id=1 B-2 A-1#id=2 C-1#id=2 A-2"]
                 -- A-1 A-1     A-2
                 -- B-1         B-2
                 -- C-1 C-1
                 it "takes first of each content " do
                   limit "^1" `shouldReturn` words "A-1#1 A-2 B-1 B-2 C-1#1"
                 it "takes firsts sorted" do
                   limit "^2" `shouldReturn` words "A-1#1 A-1#2 A-2 B-1 B-2 C-1#1 C-1#2"
                 it "with styles" do
                    limit "^1^{style}2" `shouldReturn` words "A-1#1 A-2 B-1 B-2 C-1#1"
      context "by shelf" do
          let ?boxes = ["S1 A-1 A-2 B-1"
                       ,"S2 A-2 A-3 B-1 B-2"
                       ,"S3 B-1 C-1"
                       ]
          context "{shelfname}" do
                  it "take one of each shelf" do
                     limit "^^{shelfname}1"  `shouldReturn` words "A-1 A-2 B-1"
                  it "take 2 of each shelf" do
                     limit "^^{shelfname}2"  `shouldReturn` words "A-1 A-2 A-2 A-3 B-1 C-1"
   context "with tag" do
         let ?boxes = [ "S1 B-1#id=4"
                      , "S1 A-1#id=2 A-1#id=3 A-1#id=1"
                      , "S1 B-1#id=4 B-2#id=6 B-2#id=5"
                      ]
         context "sort by priority but group by content" do
            it "take hight priority of each content" do
               limit "^[id]1"  `shouldReturn` words "A-1#1 B-1#4 B-2#5"
            it "take hight priority of each explicit content" do
               limit "^[id]1^{content}"  `shouldReturn` words "A-1#1 B-1#4 B-2#5"
            it "take hight priority of each content" do
               limit "^[id]2"  `shouldReturn` words "A-1#1 A-1#2 B-1#4 B-1#4 B-2#5 B-2#6"
            it "take hight priority of each explicit content" do
               limit "^[id]1^-{style}"  `shouldReturn` words "B-1#4 B-2#5 A-1#1"

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
   let selected = limitByNumber (traceShowId $ parseBoxNumberSelector $ drop 1 selection) boxes
   --                                                                   ^^^^^
   -- parseBoxNumberSelector doesn't expect a ^ at the begining as parseBox
   -- the boxname and the box number selector)
   return $ map ((\b -> boxStyleAndContent b <> maybe "" ("#" <>) (getTagValuem b "id"))  . fst) selected
   -- return $ map (boxStyleWithTags . fst) selected
   
   
makeShelves :: [Text] -> WH [Shelf s] s
makeShelves = mapM go where
    go name = newShelf name Nothing shelfDim shelfDim 0 DefaultOrientation ColumnFirst
    shelfDim = Dimension 300 80 150
    
    
makeBoxes :: [Text] -> WH [(Box s, Shelf s)] s
makeBoxes shelfboxess = do
   boxess <- forM shelfboxess  \shelf'boxes  -> do
                  let (shelf:boxes) = words shelf'boxes
                  mapM (go shelf) boxes
                 
   return $ concat boxess
   where go shelfname name = do
            [shelfId] <- findShelfBySelector (parseSelector shelfname)
            shelf <- findShelf shelfId
            let (style', tags) = extractTags name
                (style, drop 1 -> content) = breakOn "-" style'
            box <- newBox style content dim  up shelf [up] tags
            return (box, shelf)
         dim = Dimension 50 30 40
   

   

