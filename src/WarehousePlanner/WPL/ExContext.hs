{-# OPTIONS_GHC -Wno-deprecations #-}
module WarehousePlanner.WPL.ExContext

where
import ClassyPrelude hiding(empty, (\\), fromList, toList)
import Data.Foldable (toList)
import WarehousePlanner.Type
import WarehousePlanner.Base
import Control.Monad.State(gets)

-- | Execution Context, i.e. the select boxes shelves etc ...
-- We store ids (and not box or shelf( in purpose
-- to force a reload if necessary when narrowing 
data ExContext s = ExContext
               { ecBoxes :: InExcluded (BoxId s, Priority)
               , ecShelves :: InExcluded (ShelfId s)
               , ecParent :: Maybe (ExContext s)
               , ecSelector :: BoxNumberSelector
               , ecPartitionMode :: PartitionMode
               , ecOrientationStrategies :: [OrientationStrategy]
               }
     deriving (Show, Eq)
     
     
instance Semigroup (ExContext s) where
   ec1 <> ec2 = ExContext (ecBoxes ec1 <> ecBoxes ec2)
                          (ecShelves ec1 <> ecShelves ec2)
                          (ecParent ec1 <|> ecParent ec2)
                          (error "esSelector <> not IMPLEMENTED")
                          (min (ecPartitionMode ec1) (ecPartitionMode ec2))
                          (ecOrientationStrategies ec1 <> ecOrientationStrategies ec2)

                          
instance Monoid (ExContext s) where
   mempty = ExContext mempty mempty Nothing (BoxNumberSelector NoLimit NoLimit NoLimit) PRightOnly []
   
-- We don't update the parent in purpose
inverseBoxes :: ExContext s -> ExContext s
inverseBoxes ec = ec { ecBoxes = inverseInEx $ ecBoxes ec }

inverseShelves :: ExContext s -> ExContext s
inverseShelves ec = ec { ecShelves = inverseInEx $ ecShelves ec }
    
-- * Impure
withAll :: ExContext s
withAll = ExContext allIncluded allIncluded Nothing (BoxNumberSelector NoLimit NoLimit NoLimit) PRightOnly []
                     
-- | Boxes of the current which satisfy the given selector.
-- At the moment, it is implemented as the intersection of 
-- the context boxes and all boxes 
narrowBoxes :: BoxSelector -> ExContext s -> WH (ExContext s) s
narrowBoxes selector ec = do
   let finalSelector = combineSelector (ecSelector ec) selector
   -- traceShowM("SELECTOR", selector, ecSelector ec , " => ", finalSelector)
   ecB <- case included (ecBoxes ec) of 
     Nothing {- AllOf -} -> do
           inc <- findBoxByNameAndShelfNamesWithPriority finalSelector
           return allIncluded { included = Just inc } 
     Just inc -> do 
          (incs, exs) <- partitionBoxes finalSelector inc
          return $ InExcluded (Just incs) (Just exs)

   return $ ec { ecBoxes = fmap (first boxId) ecB, ecSelector = numberSelector finalSelector }
   
narrowShelves :: ShelfSelector -> ExContext s -> WH (ExContext s) s
narrowShelves selector ec = do
    ecS <- case included (ecShelves ec) of 
      Nothing {- AllOf -} -> do
            inc  <- findShelvesByBoxNameAndNames selector
            return allIncluded { included = Just inc }
      Just inc -> do
           (incs, exs) <- partitionShelves selector inc
           return $ InExcluded (Just incs) (Just exs)
    return ec { ecShelves = fmap shelfId ecS }

getBoxPs :: ExContext s -> WH [(Box s, Priority)] s
getBoxPs ec = do
     case included (ecBoxes ec) of
         Nothing -> findBoxByNameAndShelfNamesWithPriority selectAllBoxes
         Just incs -> mapM (firstM findBox)  incs
  where firstM f (a,b) = (,b) <$> f a

getBoxes :: ExContext s -> WH [Box s] s
getBoxes ec = map fst <$> getBoxPs ec 

  
getShelves :: ExContext s -> WH [Shelf s] s
getShelves ec = do
    sIds <- case included (ecShelves ec) of 
        Nothing -> toList <$> gets shelves
        Just sIds -> return sIds 
    mapM findShelf sIds
  

combineSelector :: BoxNumberSelector -> BoxSelector -> BoxSelector
combineSelector bns sel = let
   bns' = numberSelector sel
   in sel { numberSelector = BoxNumberSelector{ nsPerContent = combineLimitKeys (nsPerContent bns) (nsPerContent bns') 
                                              , nsPerShelf = combineLimitKeys (nsPerShelf bns) (nsPerShelf bns') 
                                              , nsTotal = combineLimitKeys (nsTotal bns) (nsTotal bns') 
                                              }
          }

-- | Combine Limit Keys but reset start and stops
combineLimitKeys :: Limit -> Limit -> Limit
combineLimitKeys NoLimit b = b
combineLimitKeys a NoLimit = a { liStart = Nothing, liEnd = Nothing} -- , liUseBase =False }
combineLimitKeys a b = b { liOrderingKey = if liUseBase b 
                                           then liOrderingKey a <> liOrderingKey b
                                           else liOrderingKey b
                         , liUseBase = liUseBase a && liUseBase b
                         }
