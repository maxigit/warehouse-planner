{-# Language ImplicitParams #-}
module Planner.ShelfOpSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Exec
import WarehousePlanner.Base
import WarehousePlanner.ShelfOp
import Planner.SpecUtil

spec :: Spec
spec = parallel pureSpec

-- {{{
pureSpec :: Spec
pureSpec = describe "dimForSplit" do
   let ?dim = mempty
   {-
       +-----------------+
       | A |   | B | ... |
       +-----------------+
       ^   ^   ^   ^     ^
       |   |   |   |     |
       |   |   |   |     +-- 300
       |   |   |   +--------  90
       |   |   +------------  60
       |   +----------------  30
       +--------------------   0


    -}
   let shouldEval ref (minX, maxX) =  do
                  sdim <- execWH (emptyWarehouse $ fromGregorian 2025 10 27) do
                         [shelf, ignore] <- makeShelves ["S", "ignore"]
                         boxes@[_,(x,_) ,_] <- makeBoxesWithPartition PRightOnly  ["S A", "S x B"]
                         -- move X to ignore to we have a gap between A and B
                         assignShelf (Just ignore) x
                         dimForSplit Nothing shelf ref
                  dLength (sMinD sdim) `shouldBe` minX
                  dLength (sMaxD sdim) `shouldBe` maxX
   it "<A" do
      "<A" `shouldEval` (0, 0)
   it "=A" do
      "=A" `shouldEval` (0, 30)
   it ">A" do
      ">A" `shouldEval` (30, 60)
   it "<B" do
      "<B" `shouldEval` (30, 60)
   it "=B" do
      "=B" `shouldEval` (60, 90)
   it ">B" do
      ">B" `shouldEval` (90, 300)
   it "before" do
       "before" `shouldEval` (0,0)
   it "content" do
       "content" `shouldEval` (90,90)
     




   
