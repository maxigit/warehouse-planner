{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-top-binds #-}
module WarehousePlanner.WPL.Exec
( runWPL
, readWPL
)
where 

import ClassyPrelude
import WarehousePlanner.WPL.Types
import WarehousePlanner.Base
import Text.Megaparsec qualified as P
import WarehousePlanner.WPL.Parser


-- Keeps selection 
data ExContext s = ExContext 
               { ecBoxes :: Seq (Box s)
               -- , ecOtherBoxes :: Seq (Box s)
               , ecShelves :: Seq (Shelf s)
               -- , ecOtherShelves :: Seq (Shelf s)
               } 
     deriving Show
pretty :: ExContext s -> String
pretty ec = "Boxes = " <> prettySeq (ecBoxes ec) <> " Shelves = " <> prettySeq (ecShelves ec) where
      prettySeq sq = case toList sq of 
                       [] -> "0"
                       x:_ -> show (length sq) <> ":" <> show x

emptyEC = ExContext mempty mempty
runWPL :: Statement -> WH () s
runWPL statement = void $ executeStatement emptyEC statement

executeStatement :: ExContext s -> Statement -> WH (ExContext s, ExContext s) s
executeStatement ec _statement =  return (ec, ec)


narrowExBoxes :: ExContext s -> BoxSelector -> WH (ExContext s, ExContext s) s
narrowExBoxes ec selector =
    case ecBoxes ec of 
        boxes | null boxes -> do 
              selected <- findBoxByNameAndShelfNames selector
              return ( ec { ecBoxes = fromList selected }
                     , ec  { ecBoxes = mempty }
                     )
        boxes -> do
              selected <- findBoxByNameAndShelfNames selector
              let (kept, left) = partitionIn boxes (fromList selected)
              return ( ec { ecBoxes = kept }
                     , ec  { ecBoxes = left }
                     )
narrowExShelves :: ExContext s -> ShelfSelector -> WH (ExContext s, ExContext s) s
narrowExShelves ec selector =
    case ecShelves ec of
            shelves | null shelves -> do
                selected <- findShelvesByBoxNameAndNames selector
                return ( ec { ecShelves = fromList selected }
                       , ec { ecShelves = mempty }
                       )
            shelves -> do
                selected <- findShelvesByBoxNameAndNames selector
                let (kept, left) = partitionIn shelves (fromList selected)
                return ( ec { ecShelves = kept }
                       , ec { ecShelves = left }
                       )

executeCommand :: ExContext s -> Command -> WH (ExContext s, ExContext s) s
executeCommand ec command = case command of
    Move boxm shelfm -> do
      boxes <- getBoxes ec boxm
      shelves <- getShelves ec shelfm
      leftOver <- moveBoxes ExitLeft PRightOnly DontSortBoxes boxes shelves
      let leftq = fromList leftOver
          (_,used) = partitionIn (ecBoxes ec) leftq
      return ( ec { ecBoxes = used }
             , ec { ecBoxes = leftq }
             )
    SelectBoxes selector -> do
      narrowExBoxes ec selector
    SelectShelves selector -> do
      narrowExShelves ec selector
       
getBoxes :: ExContext s -> Maybe BoxSelector -> WH [Box s] s
getBoxes ec selm = 
    fmap toList case selm of 
         Nothing -> return $ ecBoxes ec
         Just sel -> (ecBoxes . fst) <$> narrowExBoxes ec sel
 
getShelves :: ExContext s -> Maybe ShelfSelector -> WH [Shelf s] s
getShelves ec selm = 
    fmap toList case selm of 
         Nothing -> return $ ecShelves ec
         Just sel -> (ecShelves . fst) <$> narrowExShelves ec sel
-- | Return what is in common an what is only in the first sequence
-- ex 1234 `partitionIn` 29 -> (2, 134)
partitionIn :: forall a . Ord a => Seq a -> Seq a -> (Seq a, Seq a)
partitionIn xs ys = let
   ySet :: Set a
   ySet = setFromList $ toList ys
   in partition (`member` ySet) xs
               
  

readWPL :: MonadIO m => FilePath ->  m [Statement]
readWPL filename = do 
    content <- readFileUtf8 filename
    let e = P.runParser wplParser filename content
    case e of
       Left bundle -> error $ P.errorBundlePretty bundle
       Right statements -> return statements

