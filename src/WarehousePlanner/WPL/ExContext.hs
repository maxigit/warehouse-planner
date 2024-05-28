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
               { ecBoxes :: InExcluded (BoxId s)
               , ecShelves :: InExcluded (ShelfId s)
               , ecParent :: Maybe (ExContext s)
               }
     deriving (Show, Eq)
     
     
instance Semigroup (ExContext s) where
   ec1 <> ec2 = ExContext (ecBoxes ec1 <> ecBoxes ec2)
                          (ecShelves ec1 <> ecShelves ec2)
                          (ecParent ec1 <|> ecParent ec2)
                          
instance Monoid (ExContext s) where
   mempty = ExContext mempty mempty Nothing
   
-- We don't update the parent in purpose
inverseBoxes :: ExContext s -> ExContext s
inverseBoxes ec = ec { ecBoxes = inverseInEx $ ecBoxes ec }

inverseShelves :: ExContext s -> ExContext s
inverseShelves ec = ec { ecShelves = inverseInEx $ ecShelves ec }
    
-- * Impure
withAll :: ExContext s
withAll = ExContext allIncluded allIncluded Nothing
                     
-- | Boxes of the current which satisfy the given selector.
-- At the moment, it is implemented as the intersection of 
-- the context boxes and all boxes 
narrowBoxes :: BoxSelector -> ExContext s -> WH (ExContext s) s
narrowBoxes selector ec = do
   ecB <- case included (ecBoxes ec) of 
     Nothing {- AllOf -} -> do
           inc <- findBoxByNameAndShelfNames selector
           return allIncluded { included = Just inc } 
     Just inc -> do 
          (incs, exs) <- partitionBoxes selector inc
          return $ InExcluded (Just incs) (Just exs)

   return $ ec { ecBoxes = fmap boxId ecB }
   
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

getBoxes :: ExContext s -> WH [Box s] s
getBoxes ec = do
   bIds <- case included (ecBoxes ec) of 
                Nothing {- AllOff -} -> toList <$> gets boxes
                Just bIds -> return bIds
   mapM findBox bIds


getShelves :: ExContext s -> WH [Shelf s] s
getShelves ec = do
    sIds <- case included (ecShelves ec) of 
        Nothing -> toList <$> gets shelves
        Just sIds -> return sIds 
    mapM findShelf sIds
  


