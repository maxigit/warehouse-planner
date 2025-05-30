module WarehousePlanner.Summary
( Summary(..)
, summaryFromShelf
, summaryFromShelves
, ShelvesSummary(..)
, ratio
, makeRunsSummary
, makeBoxesSummary
)
where 

import ClassyPrelude
import WarehousePlanner.Type
import GHC.Generics
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Text (commonPrefixes)
import WarehousePlanner.Affine

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

data ShelvesSummary e f a = ShelvesSummary
               { sName :: !Text
               , sDetails :: f a
               -- lazy , sort of caches
               , sBoxSummary :: Summary
               , sShelvesSummary :: Summary
               , sExtra :: e
               }
     deriving (Functor, Foldable, Traversable)
     
instance (Semigroup e, Semigroup (f a))  => Semigroup (ShelvesSummary e f a) where
  s1 <> s2 = ShelvesSummary (commonPrefix (sName s1) (sName s2))
                            (sDetails s1        <> sDetails s2)
                            (sBoxSummary s1     <> sBoxSummary s2)
                            (sShelvesSummary s1 <> sShelvesSummary s2)
                            (sExtra s1 <> sExtra s2)
           where commonPrefix t1 t2 = 
                  case commonPrefixes t1 t2 of
                    Just (common, _, _) -> common
                    Nothing             -> t1 <> "|" <> t2

ratio :: (Summary -> Double) -> ShelvesSummary e f s -> Double
ratio f ShelvesSummary{sShelvesSummary,sBoxSummary} = case f sShelvesSummary of
  0 -> 1
  x -> f sBoxSummary / x
  

summaryFromShelf :: (Shelf s -> WH e s) -> Shelf s -> WH (ShelvesSummary e NonEmpty (Shelf s)) s
summaryFromShelf makeExtra shelf = do
   boxes <- mapM findBox (_shelfBoxes shelf)
   e <- makeExtra shelf
   return $ ShelvesSummary (shelfName shelf)
                           (shelf :| [])                          
                           (makeBoxesSummary $ toList boxes)
                           (makeShelfSummary shelf)
                           e
                           
summaryFromShelves :: Semigroup e => (Shelf s -> WH e s) -> NonEmpty (Shelf s) -> WH (ShelvesSummary e NonEmpty (Shelf s)) s
summaryFromShelves makeExtra shelves = sconcat <$> mapM (summaryFromShelf makeExtra) shelves

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
  maxCorner = case nonEmpty $ map boxAffDimension boxes of
                  Nothing -> AffDimension mempty mempty
                  Just cs -> sconcat cs
  Dimension suMaxLength suMaxWidth suMaxHeight = aTopRight maxCorner
  suSurfaceLW = boxesSurface dLength dWidth boxes
  suSurfaceLH = boxesSurface dLength dHeight boxes
  suSurfaceWH = boxesSurface dWidth dHeight boxes
  suCount = length boxes

data R' f a = R' (Runs f a)
     deriving (Functor, Foldable, Traversable)

makeRunsSummary :: Semigroup e => (Shelf s -> WH e s) -> Runs NonEmpty (Shelf s)  -> WH (Runs (ShelvesSummary e NonEmpty) (ShelvesSummary e NonEmpty (Shelf s))) s
makeRunsSummary makeExtra runs = do
  runs' <- traverseRuns (summaryFromShelf makeExtra) runs
  return $ fromRuns3 (sconcat . fmap promote) (sconcat . fmap promote) (sconcat . fmap promote) runs'


promote :: ShelvesSummary e NonEmpty a -> ShelvesSummary e NonEmpty (ShelvesSummary e NonEmpty a)
promote sum@ShelvesSummary{..} = ShelvesSummary{sDetails=pure sum , ..}


  
  

  
