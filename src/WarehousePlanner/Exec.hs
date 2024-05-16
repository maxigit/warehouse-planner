module WarehousePlanner.Exec
( execWH
, runWH
, copyWarehouse
) where

import ClassyPrelude
import WarehousePlanner.Base
import Control.Monad.State (get, put, evalStateT,runStateT, modify)
import Control.Monad.ST (stToIO)
import Unsafe.Coerce (unsafeCoerce)
import Data.Semigroup(Arg(..))

-- * Exec 
execWH :: MonadIO m => Warehouse RealWorld -> WH a RealWorld -> m a
execWH warehouse0 wh = liftIO $ stToIO $ evalStateT wh warehouse0

runWH :: MonadIO m => Warehouse RealWorld -> WH a RealWorld -> m (a, Warehouse RealWorld)
runWH warehouse0 wh = liftIO $ stToIO $ runStateT wh warehouse0

-- * Deep copy 
copyWarehouse :: forall t s . WH (WH (Warehouse s) s) t
copyWarehouse = do
  wh0 <- get
  shelfBuilders <- mapM copyShelf (toList $ shelves wh0)
  boxBuilders <- mapM (findBox >=> copyBox) (toList $ boxes wh0)

  return $ do
    put (emptyWarehouse $ whDay wh0) { boxStyling = unsafeCoerce $ boxStyling wh0
                                    , shelfStyling = unsafeCoerce $ shelfStyling wh0
                                    , boxOrientations = unsafeCoerce $ boxOrientations wh0
                                    , whCurrentEvent = whCurrentEvent wh0
                                    , whEventHistory = whEventHistory wh0
                                    }
    n'shelfIds <- sequence shelfBuilders
    shelf0 <- defaultShelf
    -- build shelf id map (from old to new one)
    let shelfIdMap = mapFromList n'shelfIds
    void $ mapM (\b -> b shelf0 shelfIdMap) boxBuilders
    modify \w -> w { whUnique = maximumEx $ fromList [0] <>  (fmap (\s -> let (ShelfId_ (Arg i _)) = shelfId s in i) (shelves w)
                                           <> fmap (\b -> let (BoxId_ (Arg i _)) = boxId b in i)  (boxes w)) }
    get


copyShelf :: ShelfId t -> WH (WH (ShelfId t, ShelfId s) s) t
copyShelf sId = do
  Shelf{..} <- findShelf sId
  return $ do
    w <- get
    let ShelfId_ (Arg i _) = _shelfId
    put  w { whUnique = i - 1 }
    nshelf <- (newShelf shelfName (Just $ intercalate "#" $ flattenTags $ shelfTag) minDim maxDim bottomOffset shelfBoxOrientator shelfFillingStrategy)
    let nId = shelfId nshelf
    updateShelf (\s ->  s {flow = flow} ) nshelf
    return (sId, nId)
  
copyBox :: Box t -> WH (ShelfId s -> Map (ShelfId t) (ShelfId s) -> WH (Box s) s) t
copyBox Box{..} = return $ \defaultShelf shelfMapId -> do
  let shelf = case boxShelf of
                Just s -> findWithDefault defaultShelf s shelfMapId 
                Nothing ->  defaultShelf
  w <- get
  let BoxId_ (Arg bId _) = _boxId
  put  w { whUnique = bId - 1 }
  newBox <- newBox boxStyle boxContent _boxDim orientation shelf boxBoxOrientations []
  updateBox (\b -> b { boxOffset = boxOffset, boxTags = boxTags, boxBreak = boxBreak, boxPriorities = boxPriorities}) newBox

