module WarehousePlanner.WPL.ExContext

where
import ClassyPrelude hiding(empty, (\\), fromList, toList)
import Data.Foldable (toList)
import WarehousePlanner.Type
import WarehousePlanner.Base
import Data.Set.Ordered
import Control.Monad.State(gets)

-- | Execution Context, i.e. the select boxes shelves etc ...

data ExContext s = ExContext
               { ecBoxes :: OSet (Box s)
               , ecShelves :: OSet (Shelf s)
               , ecParent :: Maybe (ExContext s)
               }
     deriving (Show, Eq)
     
     
instance Semigroup (ExContext s) where
   ec1 <> ec2 = ExContext (ecBoxes ec1 |<> ecBoxes ec2)
                          (ecShelves ec1 |<> ecShelves ec2)
                          (ecParent ec1 <|> ecParent ec2)
                          
instance Monoid (ExContext s) where
   mempty = ExContext empty empty Nothing
   
-- We don't update the parent in purpose
inverseBoxes :: ExContext s -> ExContext s
inverseBoxes ec = case ecParent ec of
    Nothing -> ec { ecBoxes = empty }
    Just parent -> ec { ecBoxes = ecBoxes parent \\ ecBoxes ec }

inverseShelves :: ExContext s -> ExContext s
inverseShelves ec = case ecParent ec of
    Nothing -> ec { ecShelves = empty }
    Just parent -> ec { ecShelves = ecShelves parent \\ ecShelves ec }
    
-- | Types to help dealing with the different return types
-- of misc function
data ExOperation s = NarrowBoxes [Box s]
                   | ExcludeBoxes [Box s ]
                   | NarrowShelves [Shelf s]
                   | ExcludeShelves [Shelf s]
     deriving (Show, Eq)
     
applyExOperation :: ExOperation s -> ExContext s -> ExContext s
applyExOperation op ec0 =
    let ec = ec0 { ecParent = Just ec0 }
    in case op of
        NarrowBoxes boxes -> ec { ecBoxes = fromList boxes  |/\ ecBoxes ec0 } 
             -- make sure that the boxes where already part of the context
        ExcludeBoxes boxes -> ec { ecBoxes = ecBoxes  ec0 \\ fromList boxes }
        NarrowShelves shelves -> ec { ecShelves = fromList shelves  |/\ ecShelves ec0 } 
        ExcludeShelves shelves -> ec { ecShelves = ecShelves ec0  \\ fromList shelves }
-- * Impure
getInitial :: WH (ExContext s ) s
getInitial = do
  boxIds <- gets boxes
  boxes_ <- mapM findBox (toList boxIds)
  shelfIds <- gets shelves
  shelves_ <- mapM findShelf (toList shelfIds)
  return $ ExContext (fromList boxes_)
                     (fromList shelves_)
                     Nothing
                     
-- | Boxes of the current which satisfy the given selector.
-- At the moment, it is implemented as the intersection of 
-- the context boxes and all boxes 
narrowBoxes :: BoxSelector -> ExContext s -> WH (ExContext s) s
narrowBoxes selector ec = do
   new <- findBoxByNameAndShelfNames selector
   return $ applyExOperation (NarrowBoxes new) ec
   
narrowShelves :: ShelfSelector -> ExContext s -> WH (ExContext s) s
narrowShelves selector ec = do
    new <- findShelvesByBoxNameAndNames selector
    return $ applyExOperation (NarrowShelves new) ec


getBoxes :: ExContext s -> [Box s]
getBoxes ec = toList (ecBoxes ec)

getShelves :: ExContext s -> [Shelf s]
getShelves ec = toList (ecShelves ec)


