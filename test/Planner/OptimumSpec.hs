module Planner.OptimumSpec where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Org.Types
import WarehousePlanner.Move

spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec =
  describe "cornerHull" $ do
    it "" $ do
      cornerHull [(1,1), (0,0)] `shouldBe` [(1,1)]
    it "remove hidden corners" $ do
      --    B
      --  A
      --       C
      let a = (1,2)
          b = (2,3)
          c = (4,1)
      cornerHull [a, b, c] `shouldBe` [b, c]

      
