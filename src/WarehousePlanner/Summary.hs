module WarehousePlanner.Summary
( Summary(..)
, makeShelfSummary
, ShelvesSummary(..)
)
where 

import ClassyPrelude
import WarehousePlanner.Type
import GHC.Generics
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (commonPrefixes)

data Summary = Summary
  { suVolume 
  , suMaxLength
  , suMaxWidth
  , suMaxHeight
  , suSurfaceLW
  , suSurfaceLH
  , suSurfaceWH :: Double
  , suCount :: Int
  }
  deriving (Show, Generic)
  
-- Implement Semigroup
class GS a where
 gconcat :: a -> a -> a
 
instance (Num c) => GS (K1 i c p) where
  gconcat (K1 x) (K1 y) = K1 (x + y)
  
instance GS (f p) => GS (M1 i t f p ) where
  gconcat (M1 x) (M1 y) = M1 (gconcat x y)
  
instance  (GS (f p), GS (g p)) => GS ((f :*: g) p) where
  gconcat (x :*: y) (x' :*: y') = (gconcat x x') :*: (gconcat y y')
 
instance Semigroup Summary where
  a <> b = to $ gconcat (from a) (from b)
  
instance Monoid Summary where
  mempty = Summary 0 0 0 0 0 0 0 0 

  
-- * Shelves

data ShelvesSummary a = ShelvesSummary
               { sName :: !Text
               , sShelves :: !(NonEmpty a)
               -- lazy , sort of caches
               , sBoxSummary :: Summary
               , sShelvesSummary :: Summary
               }
     deriving (Foldable)
     
instance Semigroup (ShelvesSummary a) where
  s1 <> s2 = ShelvesSummary (commonPrefix (sName s1) (sName s2))
                            (sShelves s1        <> sShelves s2)
                            (sBoxSummary s1     <> sBoxSummary s2)
                            (sShelvesSummary s1 <> sShelvesSummary s2)
           where commonPrefix t1 t2 = 
                  case commonPrefixes t1 t2 of
                    Just (common, _, _) -> common
                    Nothing             -> t1 <> "|" <> t2

ratio :: (Summary -> Double) -> ShelvesSummary s -> Double
ratio f ShelvesSummary{sShelvesSummary,sBoxSummary} = case f sShelvesSummary of
  0 -> 1
  x -> f sBoxSummary / x
  

summaryFromShelf :: Shelf s -> WH (ShelvesSummary (Shelf s)) s
summaryFromShelf shelf = do
   boxes <- mapM findBox (_shelfBoxes shelf)
   return $ ShelvesSummary (shelfName shelf)
                           (shelf :| [])                          
                           (makeBoxesSummary $ toList boxes)
                           (makeShelfSummary shelf)

makeShelfSummary :: Shelf s -> Summary
makeShelfSummary shelf = Summary{..} where
  suVolume =  volume (maxDim shelf)
  Dimension suMaxLength suMaxWidth suMaxHeight = maxDim shelf
  suSurfaceLW = suMaxLength * suMaxWidth
  suSurfaceLH = suMaxLength * suMaxHeight
  suSurfaceWH = suMaxWidth * suMaxHeight
  suCount = 1
  
makeBoxesSummary :: [Box s] -> Summary
makeBoxesSummary boxes = Summary{..} where
  suVolume = sum $ map boxVolume boxes
  maxCorner = mconcat $ map boxAffDimension boxes
  Dimension suMaxLength suMaxWidth suMaxHeight = aTopRight maxCorner
  dims = map boxDim boxes
  suSurfaceLW = sum [l * w | Dimension l w _ <- dims ]
  suSurfaceLH = sum [l * h | Dimension l _ h <- dims ]
  suSurfaceWH = sum [w * h | Dimension _ w h <- dims ]
  suCount = 1



  