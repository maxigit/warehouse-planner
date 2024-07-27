{-# Language BlockArguments #-}
module Planner.WarehouseSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Csv
import WarehousePlanner.Base
import WarehousePlanner.Tiling
import WarehousePlanner.Org.Internal
import WarehousePlanner.Exec

spec :: Spec
spec = parallel pureSpec 

pureSpec :: Spec
pureSpec = do
  expandSpecs 
  boxArrangements 
  expandAttributes
expandSpecs = describe "Expand" $ do
  it "exands brackets" $ do
    expand "a[12]" `shouldBe` [("a1", []), ("a2", [])]

  it "add tags" $ do
    expand "a[12#tag]" `shouldBe` [("a1", []), ("a2", ["tag"])]

  it "append tags" $ do
    expand "a[12#tag]#old" `shouldBe` [("a1", ["old"]), ("a2", ["tag", "old"])]
  it "expands multiple times with tags" $ do
    expand "a[12#two][XY#boo]#old" `shouldBe` [("a1X", ["old"])
                                              , ("a1Y", ["boo", "old"])
                                              , ("a2X", ["two", "old"])
                                              , ("a2Y", ["two", "boo", "old"])
                                              ]
  it "keep tags in words" $ do
    expand "a[1#tag 2]#old" `shouldBe` [("a1", ["tag","old"]), ("a2", ["old"])]

boxArrangements = describe "boxArrangements @F" $ do
  it "finds standard arrangement" $ do
    howMany (Dimension 33 1 36) 
            (Dimension 33 1 36) 
            (Dimension 10 1 13)
            `shouldBe` (HowMany 6 3 1 2)
  it "finds complex arrangement in square"  $ do
    --    1477
    --    14 8
    --    2558
    --    2 69
    --    3369
    howManyWithDiagonal (Dimension 33 1 36) 
                        (Dimension 33 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Diagonal (HowMany 9 3 1 3) 3
  it "finds complex arrangement - not big enough" $ do
    howManyWithDiagonal (Dimension 32 1 36) 
                        (Dimension 32 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Regular (HowMany 6 3 1 2)
  it "finds complex arrangement 2 squares" $ do
    --    1477adgg
    --    14 8ad h
    --    2558beeh
    --    2 69b fi
    --    3369ccfi
    howManyWithDiagonal (Dimension 66 1 36) 
                        (Dimension 66 1 36) 
            (Dimension 10 1 13)
            `shouldBe` Diagonal (HowMany 18 6 1 3) 3
  it "finds complex arrangement 2.5 square" $ do
    --  |   |        
    --  |-- |--      
    --  --| --|      
    --  | | | |      
    --  |-- |--       
    howManyWithDiagonal (Dimension 38 1 29) 
                        (Dimension 38 1 29) 
                        (Dimension 10 1 9)
            `shouldBe` Diagonal (HowMany 12 4 1 3) 2
  forM_ {- [ (l,h) | l <- [3..25] , h <- [3..25]] -} []  \(l, h) -> do
       let howManyWithDiagonalOld = error "Use old implementation"
       xit ("finds complex arrangement " <> show l <> "x" <> show h) $ do
         let box = (Dimension l 1 h)
             shelf = (Dimension 38 1 29) 
         howManyWithDiagonal shelf shelf box
                 `shouldBe` howManyWithDiagonalOld shelf shelf box


shouldExpandTo attribute expected = do
  let shelfDim = Dimension 200 100 200
      today = fromGregorian 2023 07 10
  val <- execWH (emptyWarehouse today)$ do
    a <- newShelf "Shelf S" Nothing shelfDim shelfDim 0 DefaultOrientation ColumnFirst
    _ <- newBox "Box A" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=V"]
    box <- newBox "Box B" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=X"]
    _ <- newBox "Box C" ""(Dimension 50 50 80) (readOrientation '=') a [] ["tag=Y"]
    expandAttribute box 1 attribute
  val `shouldBe` expected
expandAttributes = describe "expand box attributes" $ do
  describe "expands" do
      it  "one attribute" do
          "prefix-${shelfname}-suffix"  `shouldExpandTo` "prefix-Shelf S-suffix"
      it "two attributes" do
          "${boxname} in ${shelfname}"  `shouldExpandTo` "Box B in Shelf S"
      it "nothing" do
          "" `shouldExpandTo` ""
      it "tags" do
          "tag value $[tag]." `shouldExpandTo` "tag value X."
      it "statistics" do
          "rank = $rank[tag]." `shouldExpandTo` "rank = 2."
      it "statistics with modulo" do
          "min rank = $rank-1[tag]." `shouldExpandTo` "min rank = 1."
      it "keeps $$" do
         "^full $$match$" `shouldExpandTo` "^full $match$"
