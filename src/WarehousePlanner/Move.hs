module WarehousePlanner.Move
( aroundArrangement 
, bestArrangement
, bestPositions, bestPositions'
, stairsFromCorners
, moveBoxes
, moveSimilarBoxes
)
where 
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import qualified Prelude
import Control.Monad.State(gets)
import Data.List.NonEmpty(unzip)
import Data.List qualified as List
import WarehousePlanner.Type
import WarehousePlanner.Slices
import WarehousePlanner.SimilarBy
import WarehousePlanner.Base
import WarehousePlanner.Affine
import WarehousePlanner.Tiling

-- | Remove boxes for shelves and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement :: (Shelf' shelf, Box' box, Box' box2)
                  => AddOldBoxes 
                  -> (forall b . Box' b =>  [b s] -> [shelf s] -> WH (InExcluded (box2 s)) s)
                -> [box s] -> [shelf s] -> WH (InExcluded (box2 s)) s
aroundArrangement useOld arrangement newBoxishs shelves = do
    newBoxes <- mapM findBox newBoxishs
    boxes <- case useOld of
              NewBoxesOnly -> return newBoxes
              AddOldBoxes -> do
                          oldBoxes <- concatMap reverse `fmap` mapM findBoxByShelf shelves
                          return $ oldBoxes ++ newBoxes

    let nothing = Nothing `asTypeOf` headMay shelves -- trick to typecheck
    void $ mapM (assignShelf nothing) boxes
    -- rearrange what's left in each individual space
    -- so that there is as much space left as possible

    inEx <- arrangement boxes shelves
    s0 <- defaultShelf
    void $ mapM (assignShelf (Just s0)) $ excludedList inEx
    return $ inEx



-- | find the best way to arrange some boxes within the given space
-- For the same number of boxes. use the biggest diagonal first, then  the smallest shelf
bestArrangement :: Show a => [OrientationStrategy] -> [(Dimension, Dimension, a)] -> Dimension -> (Orientation, TilingMode, a)
bestArrangement orientations shelves box = let
    options = [ (o, tilingMode, extra, volume shelf)
              | (OrientationStrategy o  minW  maxW maxLM maxHM useIrregular) <-   orientations
              , (minShelf, shelf, extra) <- shelves
              , tilingMode0 <- if useIrregular
                                   then  [ howManyWithDiagonal minShelf shelf (rotate o box)
                                         , howManyWithSplitH minShelf shelf (rotate o box)
                                         , howManyWithSplitV minShelf shelf (rotate o box)
                                         ]
                                   else [Regular (howMany minShelf shelf (rotate o box))]
              , let tilingMode = clampTilingMode maxLM (Just (minW, maxW)) maxHM tilingMode0
              ]

    bests = 
                [ ( ( tilingMode
                    , tmLength tilingMode
                    , vol
                    )
                  , (ori, tilingMode, extra)

                  )
                 | (ori, tilingMode, extra, vol ) <- options
                 -- , let Dimension bl bh _bw = rotate ori box
                 ]
    in
        -- trace ({-show shelves ++ show box ++-}  show bests) $
        snd $ minimumByEx (compare `on` fst) bests

type SimilarBoxes s = SimilarBy Dimension (Box s)


-- | Find the  best box positions for similar boxes and a given shelf.
-- This takes into account the boxes already present in the shelf and
-- the possible orientation and shelf strategy.
bestPositions :: PartitionMode -> Shelf s -> SimilarBoxes s -> WH (Slices Double Position) s
bestPositions partitionMode shelf simBoxes = do
  let SimilarBy dim box _ = simBoxes
  boxesInShelf <- findBoxByShelf shelf
  boxo <- gets boxOrientations
  let orientations = boxo box shelf
  return $ bestPositions' partitionMode orientations shelf mempty (map boxAffDimension boxesInShelf) dim 

bestPositions' :: PartitionMode -> [OrientationStrategy] -> Shelf s -> Dimension -> [AffDimension] -> Dimension -> Slices Double Position
bestPositions' POverlap orientations shelf start used dim = let
   -- try each orientation strategy individually as if the box was empty
   -- and remove the "used" positions. Then get the best one
  solutions = [ removeUsed $ bestPositions' PRightOnly [strategy]  shelf start [] dim 
              | strategy <- orientations
              ]
  sorted = sortOn (Down . Prelude.length) solutions
  in  case sorted of
     [] -> mempty
     (best:_) -> best
  where removeUsed :: Slices Double Position -> Slices Double Position
        removeUsed = filterSlices (not . isUsed) 
        isUsed :: Position -> Bool
        isUsed (Position offset orientation) = any (affDimensionOverlap $ AffDimension offset (offset <> rotate orientation dim)) used

  
bestPositions' partitionMode orientations shelf start usedBoxes dim = let
  starti = invert start
  topRightCorners = filter (not . outOfBound)
                  . map ((starti <>) . aTopRight)
                  $ usedBoxes
  Dimension lused wused hused = maxDimension $ topRightCorners
  (bestO, tilingMode, (lused', wused', hused')) =
                      bestArrangement orientations
                                        [( minDim shelf <> starti <> used, maxDim shelf <> starti <> used, (l,w,h))
                                        -- [ (Dimension (max 0 (shelfL -l)) shelfW (max 0 (shelfH-h)), (l,h))
                                        -- | (Dimension shelfL shelfW shelfH) <- [ minDim shelf, maxDim shelf ]
                                      -- try min and max. Choose min if  possible
                                        | (l,w,h) <- let go pmode =
                                                          case pmode of
                                                            PAboveOnly -> [(0,0,hused)]
                                                            PRightOnly -> [(lused,0, 0)]
                                                            PBehind -> [(0,wused, 0)]
                                                            PBestEffort -> case map (\(x,y) -> (x,0,y)) $ bestEffort topRightCorners of
                                                                            -- remove corners if more than 3 options
                                                                            xs@(_:_:_:_) -> drop 1 $ dropEnd 1 $ xs
                                                                            xs -> xs
                                                            POr m1 m2 -> go m1 ++ go m2
                                                            POverlap -> error "POverlap not implemented"
                                                   in go partitionMode
                                        , let used = Dimension (min 0 (0-l)) (min 0 (0-w)) (min 0 (0-h))
                                        ] dim
  rotated = rotate bestO dim
  base = Dimension lused' wused' hused' <> start
  in generatePositions base (shelfFillingStrategy shelf) bestO rotated tilingMode
  where outOfBound dim = minimumEx (dimensionToList dim) <= 0


generatePositions :: Dimension -> FillingStrategy -> Orientation -> Dimension -> TilingMode -> Slices Double Position
generatePositions base fillingStrategy ori (Dimension l' w' h') (Regular HowMany{..}) = 
  case fillingStrategy of
    ColumnFirst -> let
     mkPos il ih iw = (k2w iw, Position (mkDim (i2l il) (k2w iw) (j2h ih)) ori)
     in buildSlices perLength perHeight perDepth i2l (\_ j -> j2h j) mkPos 
    RowFirst -> let
     mkPos ih il iw = (k2w iw, Position (mkDim (i2l il) (k2w iw) (j2h ih)) ori)
     in buildSlices perHeight perLength perDepth j2h (\_ i -> i2l i) mkPos 
  where
     i2l i = l' * fromIntegral i + dLength base
     j2h j = h' * fromIntegral j + dHeight base
     k2w k = w' * fromIntegral k + dWidth base
     mkDim l w h = Dimension l w h

generatePositions base fillingStrategy ori boxDim (Diagonal HowMany{..} diag) =
  case fillingStrategy of
    ColumnFirst -> let
     mkPos il ih iw = let 
            (dim, turned) = mkOffset il iw ih
            in (dWidth dim, Position dim $ newOrientation turned)
     mkl i = dLength . fst $ mkOffset i 0 0
     mkh i j = dHeight . fst $ mkOffset i j 0
     in buildSlices perLength perHeight perDepth mkl mkh mkPos 
    RowFirst -> let
     mkPos ih il iw = let 
            (dim, turned) = mkOffset il iw ih
            in (dWidth dim, Position dim $ newOrientation turned)
     mkl i j = dLength . fst $ mkOffset j i 0
     mkh i = dHeight . fst $ mkOffset 0 i 0
     in buildSlices perHeight perLength perDepth mkh mkl mkPos 
  where newOrientation t = if t 
                           then  (rotateO ori)
                           else ori
        mkOffset i j k = first (base <>) $ indexToOffsetDiag boxDim diag (i, j, k)

generatePositions base fillingStrategy ori boxDim (TilingCombo dir m1 m2) = let
  positions1 = generatePositions base fillingStrategy ori boxDim m1
  positions2 = generatePositions base2 fillingStrategy (rotateO ori) (rotate tiltedRight boxDim) m2
  Dimension l w h = tmBoundingBox m1 boxDim
  base2 = base <> case ( dir) of
               Horizontal -> Dimension l 0 0 
               Depth -> Dimension 0 w 0
               Vertical -> Dimension 0 0 h
  in positions1 <> positions2

-- * Find  the corners of the boxes which are enough
-- to describe the "stair" hull of all of the top right corner
-- of the given boxes.
-- For example
--
--       B
--    A
--            C
--            D
--
-- Will return B nd C
cornerHull :: [(Double, Double)] -> [(Double, Double)]
cornerHull corners = let
  -- allCorners = map boxCorner boxes
  -- boxCorner box = boxDim box <> boxOffset box
  -- we sort them in reverse order
  -- in exapmle C D B A
  sorted = sortOn Down corners
  go corner [] = [corner]
  go (x, y)  s@((x0,y0):_) = 
    if y > y0 && x < x0
    then ((x, y): s)
    else s
  in List.foldl (flip go) [] sorted


-- | Creates the "inner" corners of a "stairs"
--  (the Xs fro the '.')
-- X   .
--     X     .
--
--           X     .
--                 X
stairsFromCorners :: [(Double, Double)] -> [(Double, Double)]
stairsFromCorners corners =
  let (xs, ys) = unzip corners
  in zipWith (,) (0:xs) (ys ++ [0]) 


bestEffort :: [Dimension] -> [(Double, Double)]
bestEffort boxes = let
  allCorners = map xy boxes
  xy (Dimension x _ y ) = (x,y)
  in stairsFromCorners $ cornerHull allCorners
  
-- | Move boxes of similar size to the given shelf if possible
moveSimilarBoxes :: (Shelf' shelf) => ExitMode -> PartitionMode -> SimilarBoxes s -> [shelf s] -> WH (Maybe (SimilarBoxes s), Maybe (SimilarBoxes s)) s
moveSimilarBoxes exitMode partitionMode boxes shelves' = do
  shelves <- mapM findShelf shelves'
  positionss <- mapM (\s -> bestPositions partitionMode s boxes) shelves
  let    positionsWithShelf = combineSlices exitMode $ zip shelves positionss
  assignBoxesToPositions positionsWithShelf boxes
  
-- | Sort positions (box offsets), so that they are in order to be assigned
-- according to the 'exitmode' and filling strategy.
--
--      ExitLeft RowFirst  : shelf -> height
--      7 8  | 9 
--      -----+--
--           | 6 
--      4  5 |  
--      1  2 | 3
--
--      ExitLeft ColumFirst  : length -> shelf
--      7 8  | 9 
--      -----+--
--           | 6 
--      2  4 |  
--      1  3 | 5
--
--      ExitOnTop RowFirst : shelf -> height
--      5 6  | 9 
--      -----+--
--           | 8 
--      3  4 |  
--      1  2 | 7
--
--      ExitOnTop ColumFirst : length -> shelf
--      3 4  | 9 
--      -----+--
--           | 8 
--      2  6 |  
--      1  5 | 7
--
-- depending on exitMode and their filling strategy,
-- consecutives shelves must be seen at one or not.
-- The sorting can then be done by group of shelf with the same
-- strategy.
combineSlices :: ExitMode -> [(Shelf s, Slices Double Position)] -> Slices (Int, Double, Int) (Shelf s, Position)
combineSlices exitMode shelf'slicess = let
  -- assign a number for each shelf and then
  -- reuse the same number within a group if needed.
  -- This way 1 2 3 4 5 will become 1 2 3 4 5 
  -- if shelves 3 and 4 are need to be filled togother
  withN = zipWith (\s i -> first (i,) s) shelf'slicess [(1::Int)..]
  sameStrategy = groupSimilar (shelfFillingStrategy . snd . fst) withN
  withGroupN = map adjustN sameStrategy
  -- adjustN :: SimilarBy FillingStrategy (_Shelf, Slices Double Position) -> Slices (Int, Double) (_Shelf, Position)
  adjustN (SimilarBy strategy s'is1 s'i'slices) = 
    let firstI = fst (fst s'is1)
        adjust i d = if (exitMode, strategy) `elem` [(ExitOnTop, ColumnFirst), (ExitLeft, RowFirst)]
                   then (firstI, d, i) -- sames "group" , within a group fill slice then go to the next shelf
                   else (i, d, i) -- fill shelf first, then go to the next one
    in [ bimap (adjust i) (shelf,) slices
       | ((i, shelf), slices) <- s'is1 : s'i'slices
       ]
  in foldMap concat withGroupN
-- | Assign boxes to positions in order (like a zip) but with respect to box breaks.
-- (skip to the next column if column break for example)
assignBoxesToPositions :: Slices k (Shelf s, Position) -> SimilarBoxes s -> WH (Maybe (SimilarBoxes s), Maybe (SimilarBoxes s)) s
assignBoxesToPositions slices simBoxes = do
  let boxes = unSimilar simBoxes
  let go _ _ [] = return []
      go prevShelf slices allboxes@(box:boxes) =
        case (unconsSlicesTo fst prevShelf (boxBreak box) slices) of
             Nothing -> return allboxes
             Just ((shelf'pos), _, newSlices) ->
              shiftBox box shelf'pos >> go (Just $ fst shelf'pos) newSlices boxes 
      shiftBox box (shelf, Position offset orientation) = do
            _ <- updateBox (\box_ -> box_ { orientation = orientation
                                          , boxOffset = offset}) box
            assignShelf (Just shelf) box
  leftOver <- go Nothing (numSlices slices) boxes 
  return $ splitSimilar (length boxes - length leftOver) simBoxes

-- Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
moveBoxes :: (Box' box , Shelf' shelf) => ExitMode -> PartitionMode -> SortBoxes -> [box s] -> [shelf s] -> WH (InExcluded (Box s)) s

moveBoxes exitMode partitionMode sortMode bs ss = do
  boxes <- mapM findBox bs
  let layers = groupBy ((==)  `on` boxBreak)
               $ (if sortMode == SortBoxes then sortOnIf boxGlobalRank else id)
               $ boxes
      boxGlobalRank box = (boxGlobalPriority box, boxStyle box, boxStylePriority box,  _boxDim box, boxContent box, boxContentPriority box)
      boxBreak box = (boxStyle box, _boxDim box)
      -- \^ we need to regroup box by style and size
      -- However we take into the account priority within the style before the dimension
      -- so that we can set the priority
        
  (unzip -> (ins, exs))  <- forM layers $ \layer -> do
    let groups = groupSimilar _boxDim layer
    -- forM groups $ \(SimilarBy dim _ boxes) -> traceShowM ("  GROUP", dim, 1 + length boxes)
    -- traceShowM ("GRoups", length groups, map (\(SimilarBy dim g1 g ) -> (show $ length g + 1, show . _roundDim $ dim )) groups)
    moved'lefts <- mapM (\g -> moveSimilarBoxes exitMode partitionMode g ss) groups
    let (moveds, lefts) = unzip moved'lefts
    return (concatMap unSimilar $ catMaybes moveds, concatMap unSimilar $ catMaybes lefts)
  return $ InExcluded (Just $ concat ins) (Just $ concat exs)

