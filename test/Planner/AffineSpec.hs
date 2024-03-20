module Planner.AffineSpec (spec) where

import ClassyPrelude
import Test.Hspec
import WarehousePlanner.Affine
import WarehousePlanner.Type
import Data.Foldable qualified as F

spec :: Spec
spec = parallel pureSpec

pureSpec :: Spec
pureSpec = do
  affDimensions
  tiling

-- * Afine
affDimensions :: Spec
affDimensions = do
  describe "AffDimensions" do
   context "inAffDimension" do
     let aff = AffDimension bottomLeft topRight
         bottomLeft = Dimension 10 20 30
         topRight = Dimension 20 30 50 
     it "True in the middle" do
        inAffDimension (Dimension 15 25 40) aff `shouldBe` True
     it "True in the left edge" do
        inAffDimension (Dimension 10 25 40) aff `shouldBe` True
     it "False in the right edge" do
        inAffDimension (Dimension 15 25 50) aff `shouldBe` False
     it "False outside" do
        inAffDimension (Dimension 35 25 40) aff `shouldBe` False
   context "overlap" do
     --         
     --           C       D
     --                       N
     --       P       M
     --         
     --    O      A       B
     let o = Dimension 0 0 0
         p = Dimension 5 5 1
         a = Dimension 10 0 0
         b = Dimension 20 0 0
         c = Dimension 10 30 10
         d = Dimension 20 30 10
         m = Dimension 15 15 5
         n = Dimension 25 15 7
     context "intersection" do
       it "OC & OP == OP" do
          affDimensionIntersection (AffDimension o c) (AffDimension p c) `shouldBe` Just (AffDimension p c)
       it "OM & AC" do
          affDimensionIntersection (AffDimension o m) (AffDimension a d) `shouldBe` Just (AffDimension a (Dimension 15 15 5))
     it "overlaps" do
       affDimensionOverlap (AffDimension o m) (AffDimension a d) `shouldBe` True
     it "... reversed" do
       affDimensionOverlap (AffDimension a d) (AffDimension o m) `shouldBe` True
     it "doesn't do" do
       affDimensionOverlap (AffDimension o c) (AffDimension m d) `shouldBe` False
       affDimensionOverlap (AffDimension m d) (AffDimension o c) `shouldBe` False
     it "doesn't if edge touches" do
        affDimensionOverlap (AffDimension o c) (AffDimension a m) `shouldBe` False
        affDimensionOverlap (AffDimension a m) (AffDimension o c) `shouldBe` False
     it "True for crosses" do
        affDimensionOverlap (AffDimension p n) (AffDimension a d) `shouldBe` True
        affDimensionOverlap (AffDimension a d) (AffDimension p n) `shouldBe` True
      
-- * Tiling
tiling :: Spec
tiling = do
  describe "tiling" do
    let makeBox name l w  x y = Box{..} where
                _boxDim = Dimension l w 1
                boxOffset = Dimension x y 0
                boxStyle = name
                boxContent = ""
                orientation = up 
                boxBreak = Nothing
                boxTags = mempty
                boxPriorities = (100, 100, 100)
         --     +-----+----+------+
         --     |     | BC | B    |
         --     +-----+----+------+
         --     |A   D|ABCD| B D  |
         --     +-----+----+------+
         --     |A   E|A C |      |
         --     +-----+----+------+


        a = makeBox "A" 2 2 0 0
        b = makeBox "B" 2 2 1 1
        c = makeBox "C" 1 3 1 0
        d = makeBox "D" 3 1 0 1
        e = makeBox "E" 0.5 0.5 0.1 0.1
        surface = boxesSurface dLength dWidth
    context "surfaces" do
       it "A = 4" do surface [a] `shouldBe` 4
       it "B = 4" do surface [b] `shouldBe` 4
       it "C = 3" do surface [c] `shouldBe` 3
       it "D = 3" do surface [d] `shouldBe` 3
       it "E = 1/4" do surface [e] `shouldBe` 1/4
    context "groupByTiles" do
      let groupBoxes boxes = map (fmap (map boxStyle)) $ groupBoxesByTile dLength dWidth boxes 
          tilesFor boxes = flatten . sort . map snd $ groupBoxes boxes
          flatten = unwords . map (sconcat . sort)

      it "A & B Corner " do
         tilesFor [a, b] `shouldBe` "A A A AB B B B"
         surface [a, b] `shouldBe` 7

      it "C & D Cross " do
         tilesFor [c, d] `shouldBe` "C C CD D D"
         surface [c, d] `shouldBe` 5
      it "A & E included " do
         tilesFor [a, e] `shouldBe` "A A A A A A A A AE"
         -- ^ 9 tiles  
         surface [a, e] `shouldBe` surface [a]
      it "B & E excluded " do
         tilesFor [b, e ] `shouldBe` "B E"
         surface [b, e] `shouldBe` surface [b] + surface [e]
         
         
