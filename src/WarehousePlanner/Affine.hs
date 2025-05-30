module WarehousePlanner.Affine
( AffDimension(..)
, affDimensionOverlap
, affDimensionIntersection
, boxAffDimension
, groupBoxesByTile
, tileSurface
, boxesSurface
, positionToAffine
, affineToDimension
)
where 

import ClassyPrelude hiding(tail)
import WarehousePlanner.Type
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.List (nub, tail)

-- * Dimensions
data AffDimension = AffDimension { aBottomLeft , aTopRight :: Dimension }
     deriving (Eq, Show, Ord)
     
instance Semigroup AffDimension where 
         (AffDimension bl tr)  <> (AffDimension bl' tr') = AffDimension (minDimension [bl, bl']) (maxDimension [tr, tr'])
  
-- * Affine
    
-- | Check weither two rectangle overlaps
affDimensionOverlap :: AffDimension -> AffDimension -> Bool
affDimensionOverlap a b = isJust $ affDimensionIntersection a b
-- | Intersection of two aff if not null
affDimensionIntersection :: AffDimension -> AffDimension -> Maybe AffDimension
affDimensionIntersection a b = let
  bottomLeft = maxDimension [aBottomLeft a, aBottomLeft b]
  topRight = minDimension [aTopRight a, aTopRight b]
  --       ^^^^^^^^^^^
  --       The intersection is just the min/max of all coordinate
  --       
  --           ---+
  --              |
  --         +--- |     ---  <-- min bottom
  --         | ===+  => ===  <-- max bottom
  --         | 
  --         +===
  --            
  --           ---+
  --              |
  --           ===+     ===  <-- min bottom
  --         +---    => ---  <-- max bottom   REJECTED min > max
  --         | 
  --         +===
  --            
  --         +--- 
  --         |     
  --         | ---+     ---  <-- min bottom
  --         | ===+  => ===  <-- max bottom
  --         | 
  --         +===
  --            
  --  We just need to check it is not null or flat
  positive x y = x > y + 1e-5
  in if and $ zipWith positive (dimensionToList topRight) (dimensionToList bottomLeft)
     then Just (AffDimension bottomLeft topRight)
     else Nothing

affineToDimensions :: AffDimension -> [Dimension]
affineToDimensions rec = let
  Dimension x' y' z' = aBottomLeft rec
  Dimension x'' y'' z'' = aTopRight rec
  in [ Dimension x y z 
     | x <- [x',x'']
     , y <- [y', y'']
     , z <- [z', z'']
     ]

affineToDimension :: AffDimension -> Dimension
affineToDimension rec = aTopRight rec <> invert (aBottomLeft rec)

positionToAffine :: Dimension -> Position -> AffDimension
positionToAffine dim Position{..} = AffDimension pOffset (pOffset <> rotate pOrientation dim)

-- * Boxes operations
boxAffDimension :: Box s -> AffDimension
boxAffDimension b = AffDimension (boxOffset b) (boxOffset b <> boxDim b)


-- | Generate a list of tiles and all the boxes intersecting that tile
-- for example
-- 
--          +------------------+
--          |A                 |
--          |          +----------------+
--          |          |B      |        |
--          |          |       |        |
--          |          +----------------+
--          |                  |
--          +------------------+
--          
--  generate 7 tiles
--          +----------+-------+.........
--          |          |  A    |   0    .
--          |          +----------------+
--          |    A     |       |        |
--          |          |  AB   |   B    |
--          |          +----------------+
--          |          |   A   |   0    .
--          +----------+-------+.........
--          
--          The tile 3 and 9 are empty (and as such removed)
--          Tile 5 contains A nd B
groupBoxesByTile :: (Dimension -> Double)  -> (Dimension -> Double) -> [Box s] -> [ (((Double, Double), (Double, Double)),  NonEmpty (Box s)) ]
groupBoxesByTile px py boxes = let
  box'affs = [ (box, boxAffDimension box) | box <- boxes ]
  points  = concatMap (affineToDimensions . snd) box'affs
  tiles = tilesFromPoints px py points
  tilesWithBoxes =  map boxesFor tiles
  -- check if tile intersect with (or is inside) the box
  boxesFor t = let keep = isBoxInTile t . snd
                   toKeep = filter keep box'affs
               in (t, map fst toKeep)
  isBoxInTile ((x,y), (x', y')) aff = affDimensionOverlap (AffDimension (Dimension x y 0) (Dimension x' y' 1))
                                                          aff
  in mapMaybe (traverse nonEmpty) tilesWithBoxes
  
tilesFromPoints :: (Dimension -> Double) -> (Dimension -> Double) -> [Dimension] -> [ ((Double, Double), (Double, Double)) ]
tilesFromPoints px py points = let 
  xs = nub $ sort $ map px points
  ys = nub $ sort $ map py points
  in [ ((x, y), (x', y'))
     | (x, x') <- zip xs (tail xs)
     , (y, y') <- zip ys (tail ys)
     ]
     
tileSurface :: ((Double, Double), (Double, Double)) -> Double
tileSurface ((x, y), (x', y')) = (x'-x) * (y' -y)

boxesSurface :: (Dimension -> Double) -> (Dimension -> Double) -> [Box s] -> Double
boxesSurface px py boxes = let
  tiles = groupBoxesByTile px py boxes
  in sum $ map (tileSurface . fst) tiles
