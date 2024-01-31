{-# LANGUAGE DerivingVia #-}
module WarehousePlanner.Types
( Shelves(..)
, ratio
, ofBoxes
, ofShelves
, MeasureType
)
where

import ClassyPrelude
import WarehousePlanner.Base
import qualified Data.Map as Map
import Brick
import Data.List.NonEmpty
import Data.Semigroup (Sum(..))
import Data.Text (commonPrefixes)



type Run s = Shelves (Bay s)
type Bay s = Shelves (Shelf s)
type Runs s = Shelves (Run s)
         
data MeasureType = Volume
                 | MaxLength
                 | MaxWidth
                 | MaxHeight
                 | FrontSurface -- Σ lxw
                 | MaxFrontSurface -- MaxL * MaxH
     deriving (Show, Eq, Ord, Enum, Bounded)
     
     
newtype Measures = Measures (Map MeasureType Double)
  deriving (Show, Semigroup, Monoid)
           via (Map MeasureType (Sum Double))

  
  
measure :: MeasureType -> Measures -> Double
measure k (Measures m) = Map.findWithDefault 0 k m
         
data Shelves a = Shelves
               { sName :: !Text
               , sShelves :: !(NonEmpty a)
               -- lazy , sort of caches
               , sBoxMeasure :: Measures
               , sShelvesMeasure :: Measures
               }
     deriving (Functor, Foldable)
     
instance Semigroup (Shelves a) where
  s1 <> s2 = Shelves (commonPrefix (sName s1) (sName s2))
                     (sShelves s1        <> sShelves s2)
                     (sBoxMeasure s1     <> sBoxMeasure s2)
                     (sShelvesMeasure s1 <> sShelvesMeasure s2)
           where commonPrefix t1 t2 = 
                  case commonPrefixes t1 t2 of
                    Just (common, _, _) -> common
                    Nothing             -> t1 <> "|" <> t2

ofBoxes :: MeasureType -> Shelves s -> Double
ofBoxes m shelves = measure m (sBoxMeasure shelves)

ofShelves :: MeasureType -> Shelves s -> Double
ofShelves m shelves = measure m (sShelvesMeasure shelves)

ratio :: MeasureType -> Shelves s -> Double
ratio m shelves = case ofShelves m shelves of
  0 -> 1
  x -> ofBoxes m shelves / x



     

     
