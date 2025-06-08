{-# LANGUAGE ScopedTypeVariables #-}
module WarehousePlanner.Move
( aroundArrangement 
, bestArrangement
, bestPositions, bestPositions'
, cornerHull
, stairsFromCorners
, generatePositions
, moveBoxes
, moveSortedBoxes
-- , moveSimilarBoxes
, moveAndTag
, moveToLocations
, splitTagsAndLocation
-- reexport
, withAll
-- internal
, addSlotBounds
, SlotBounds(..)
)
where 
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import Control.Monad.State(gets)
import Control.Monad hiding(mapM_,foldM)
import Data.List.NonEmpty(unzip, NonEmpty(..), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List qualified as List
import WarehousePlanner.Type
import WarehousePlanner.Slices
import WarehousePlanner.SimilarBy
import WarehousePlanner.Base
import WarehousePlanner.Selector
import WarehousePlanner.Affine
import WarehousePlanner.Tiling
import WarehousePlanner.WPL.ExContext
import Data.Text(uncons)
import Data.Foldable qualified as F
import Data.Map qualified as Map
import Data.Semigroup (Arg(..))
import Control.Monad.State(evalState, get, modify)

-- | Remove boxes for shelves and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement :: (Shelf' shelf, Box' box, Box' box2)
                  => forall s . AddOldBoxes 
                  -> (forall b . Box' b =>  [b s] -> [shelf s] -> WH (InExcluded (box2 s)) s)
                -> [box s] -> [shelf s] -> WH (InExcluded (box2 s)) s
aroundArrangement useOld arrangement newBoxishs shelves =  do
  inEx <- aroundArrangementWithP useOld arrangement (map (,()) newBoxishs) shelves
  return $ fmap fst inEx

aroundArrangementWithP :: (Shelf' shelf, Box' box, Box' box2) => forall s p . AddOldBoxes -> (forall b . Box' b =>  [b s] -> [shelf s] -> WH (InExcluded (box2 s)) s) -> [(box s, p)] -> [shelf s] -> WH (InExcluded (box2 s, p)) s
aroundArrangementWithP useOld arrangement newBoxishs shelves = do
    newBox'Ps <- mapM (firstM findBox) newBoxishs
    let -- priorityMap :: Map (BoxId s) p
        priorityMap =  Map.fromList $ map (first boxId) newBox'Ps
        newBoxes = map fst newBox'Ps
        boxOrder b = let Dimension l w h = boxOffset b
                     in (l,h,w)
    boxesBefore <- case useOld of
              NewBoxesOnly -> return newBoxes
              AddOldBoxes -> do
                          allOldBoxes <- concatMap (sortOn boxOrder) `fmap` mapM findBoxByShelf shelves
                          -- make sure news and old boxes don't have boxes in commun
                          let oldBoxes = filter ((`notMember` priorityMap) . boxId) allOldBoxes
                          return $ oldBoxes ++ newBoxes

    let nothing = Nothing `asTypeOf` headMay shelves -- trick to typecheck
        setPriority b = case lookup (boxId b) priorityMap of
                         Nothing -> Nothing
                         Just p -> Just (b, p)
    boxes <-  mapM (assignShelf nothing) boxesBefore
    -- rearrange what's left in each individual space
    -- so that there is as much space left as possible

    inEx <- arrangement boxes shelves
    s0 <- defaultShelf
    void $ mapM (assignShelf (Just s0)) $ excludedList inEx
    return $ inEx { included = mapMaybe setPriority <$> (included inEx), excluded = mapMaybe setPriority <$> (excluded inEx) }



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

type SimilarBoxes s = SimilarBy (Dimension, Text) (Box s)
type BoxesOrPos s = OrPos (Box s)
type OrPos b = Either (NonEmpty b) Position


-- | Find the  best box positions for similar boxes and a given shelf.
-- This takes into account the boxes already present in the shelf and
-- the possible orientation and shelf strategy.
-- Returns a slices of either an available position or the list of boxes overlapping with the position.
bestPositions :: PartitionMode -> Shelf s -> SimilarBoxes s -> WH (Slices Double Position) s
bestPositions partitionMode shelf simBoxes = do
  bOrPs <- bestPositionOrBoxes partitionMode shelf simBoxes
  return . snd $ partitionEitherSlices bOrPs

bestPositionOrBoxes :: PartitionMode -> Shelf s -> SimilarBoxes s -> WH (Slices Double (BoxesOrPos s)) s
bestPositionOrBoxes partitionMode shelf simBoxes = do
  let SimilarBy (dim,_) box _ = simBoxes
  boxesInShelf <- findBoxByShelf shelf
  boxo <- gets boxOrientations
  let orientations = boxo box shelf
  return $ bestPositions' boxAffDimension partitionMode orientations shelf mempty boxesInShelf dim 

bestPositions' :: forall b s . (b -> AffDimension ) -> PartitionMode -> [OrientationStrategy] -> Shelf s -> Dimension -> [b] -> Dimension -> Slices Double (OrPos b)
bestPositions' getAff pmode orientations shelf start used dim | isOverlap pmode = let
   -- try each orientation strategy individually as if the box was empty
   -- and remove the "used" positions. Then get the best one
  solutions = [ 
              justifyPositions
              $ fmap isUsed
              $ bestPositionsWithOffset offset strategy
              | strategy <- orientations
              , let boxLength = dLength $ rotate (osOrientations strategy) dim
              , offset <- case pmode of
                           POverlap OAligned -> [ 0
                                                , boxLength / 3
                                                , boxLength/ 2
                                                , -- align right
                                                  dLength (minDim shelf) `modFload` boxLength
                                                ]
                           _ -> [0]
              ]
  sorted = sortOn (Down . F.length . snd . partitionEitherSlices) solutions
  in  case sorted of
     [] -> mempty
     (best:_) -> best
  where overlap :: Position -> b -> Bool
        overlap pos = \box -> let
                used = getAff box
                in affDimensionOverlap (positionToAffine dim pos)  used
         -- return the position if empty or the boxes overlapping it if any               isUsed :: Position -> BoxesOrPos s
        isUsed (Right pos) = let 
           overlappings = filter  (overlap pos) used
           in case nonEmpty overlappings of
                Nothing -> Right pos
                Just ne -> Left ne
        isUsed left = left
        isOverlap = \case 
                     POverlap _ -> True
                     PSortedOverlap -> True
                     _ -> False
        justifyPositions = case pmode of
                             POverlap ORight -> justifyRight dim (minDim shelf)
                             POverlap OAligned -> \case 
                                  slices -> justifyAlign (map getAff used) dim slices
                             _ -> id
        bestPositionsWithOffset offset strategy = 
            let newShelf = shelf { minDim = minDim shelf <> shrink
                                 , maxDim = maxDim shelf <> shrink 
                                 }
                shrink = Dimension (-offset) 0 0
                positions = bestPositions' getAff PRightOnly [strategy] newShelf start [] dim
                offsetBack = fmap (\pos -> pos {pOffset = pOffset pos <> (Dimension offset 0 0)})
            in fmap offsetBack positions
        modFload r q = let p = floor(r / q)
                       in r - fromIntegral p * q
                             
                      

bestPositions' getAff partitionMode orientations shelf start usedBoxes dim = let
  starti = invert start
  topRightCorners = filter (not . outOfBound)
                  . map ((starti <>) . aTopRight)
                  $ map getAff usedBoxes
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
                                                            PCorner n -> let corners = map (\(x,y) -> (x,0,y)) $ bestEffort topRightCorners
                                                                         in case drop (n-1) corners of
                                                                                 (c:_) -> [c]
                                                                                 _ -> take 1 corners
                                                            POr m1 m2 -> go m1 ++ go m2
                                                            POverlap _ -> error "POverlap not implemented"
                                                            PSortedOverlap -> error "PSortedOverlap not implemented"
                                                   in go partitionMode
                                        , let used = Dimension (min 0 (0-l)) (min 0 (0-w)) (min 0 (0-h))
                                        ] dim
  rotated = rotate bestO dim
  base = Dimension lused' wused' hused' <> start
  in fmap Right $ generatePositions base (shelfFillingStrategy shelf) bestO rotated tilingMode
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

-- | Offset positions to align with the right of the given dimension
justifyRight :: Dimension -> Dimension -> Slices Double (OrPos b) -> Slices Double (OrPos b)
justifyRight box shelf slices = let
   (_,poss) = partitionEitherSlices slices
   pos = F.toList poss
   in case pos of
           [] -> mempty
           _ -> let maxL = maximumEx (map (dLength . aTopRight . positionToAffine box) pos)
                    offset = Dimension (max 0 $ dLength shelf - maxL) 0 0 
                in  fmap (fmap (\Position{..} -> Position {pOffset=pOffset <> offset,..})) slices
-- | Offset positions to align the first available slot
-- with the right most corner
justifyAlign :: [AffDimension] -> Dimension -> Slices Double (OrPos b) -> Slices Double (OrPos b)
justifyAlign used box slices = let
   (_,poss) = partitionEitherSlices slices
   in case sortOn (dLength . pOffset) $ F.toList poss of
        [] -> slices
        firstSlot:_ -> -- find all the used box which are left of the first slot
                      {-
                                
                                
                                +-----+-----+-----+
                                |     |     |     |
                                | YYYY|Y    |     |
                                +-----+-----+-----+
                                |.XXX.|.....|     |
                                |.XXX.|.....|     |  . zone
                                +-----+-----+-----+
                                | XXX |     |     |
                                | XXXA|AA   |     |
                                +-----+-----+-----+
                                     ^
                                     +--- offset
                                Only X overlap
                       -}
                       let leftOf = filter (affDimensionOverlap zone) used
                           firstOffset = pOffset firstSlot
                           zone = AffDimension (Dimension 0 0 (dHeight $ pOffset firstSlot)) (aTopRight $ positionToAffine box firstSlot)
                           firstL = dLength firstOffset
                           maxOffset = case fromNullable $ map (dLength . aTopRight) leftOf of
                                            Nothing -> firstL
                                            Just ls-> maximum ls
                           offset = Dimension (maxOffset - firstL) 0 0 
                       in  fmap (fmap (\Position{..} -> Position {pOffset=pOffset <> offset,..})) slices

           
   
 
-- * Find  the corners of the boxes which are enough
-- to describe the "stair" hull of all of the top right corner
-- of the given boxes.
-- 
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
moveSimilarBoxes :: (Shelf' shelf) => forall s . PartitionMode -> SimilarBoxes s -> [(ExitMode, [shelf s])] -> WH (Maybe (SimilarBoxes s), Maybe (SimilarBoxes s)) s
moveSimilarBoxes partitionMode boxes exit'shelves' = do
  slotGroups :: [(ExitMode, [(Shelf s, _)])] <- forM exit'shelves' \(exitMode, shelves') ->  do
     shelves <- mapM findShelf shelves'
     pos'overlaps <- mapM (\s -> bestPositionOrBoxes partitionMode s boxes) shelves
     return (exitMode, zip shelves pos'overlaps)
  -- ^ available positions not linked at the moment to any real boxes
  -- this is a list whith one set of slice per shelf
  let positionsWithShelf :: Slices SlotPriority (Shelf s, BoxesOrPos s)
      positionsWithShelf = combineSlicess slotGroups
  --  ^^^^^^^^^^^^^^^^^                             ^^^^^^^^^^^^^^^^^^^^^
  --       |                                              |
  --       +-- one set of slices combining                |
  --           the different shelves in the               |
  --           appropriate order                          |
  --                                                      |
  --       pair each slices with its original shelf ------+
  -------------------------------------------------------------
  --  vvvvvvvvvvvvvv partition positions and boxes
  --  in the case of sortedoverlap each group corresponds
  --  to a consecutive set of slots and the boxes allowed to go with it depending
  --  on the ordering constraints.
  --  for example six box A B C D X Y Z  with 5 position might be split in 
  --   (A B C , 1 2 ) and ( X Y Z , 3 4 5) because slot 3 needs box >= M
  --   In that case C won't be moved (not enough slought) even though without ordering
  --   constraints it would have been Z.
  --  the normal behavior is to create one group.
      posWithShelf'boxesz :: [ ( Slices SlotPriority (Shelf s, Position)
                               , SimilarBoxes s
                               )
                             ]
      posWithShelf'boxesz = case partitionMode of
         PSortedOverlap -> forSortedOverlap positionsWithShelf boxes
         _              -> [(snd $ partitionEitherSlices $ fmap sequenceA positionsWithShelf, boxes)]
  toUnzip <- mapM (\(pws, bs) ->  assignBoxesToPositions pws bs) posWithShelf'boxesz
  let (lefts, rights) = unzip toUnzip
      tomaybe xms = case catMaybes xms of
                      [] -> Nothing 
                      (x:xs) -> foldM (\m s -> unsplitSimilar m s) x xs
  return (tomaybe lefts, tomaybe rights)

-- | Allow anything to be sorted by the given index (argument).
type ArgI = Arg Int

argument :: ArgI a -> Int
argument (Arg i _) = i

argued :: ArgI a -> a 
argued (Arg _ x) = x

-- | Instead of just ordering slot and slices
-- by their geometric position (a Double)
-- the length for a slice, the height for a slot etc 
-- we need to be able to force the order to take into account the partition mode.
-- In ExitLeft for example , slices needs to be sorted by shelf first
-- in that case master priority should be the shelf position.
-- In ExitOnTop, the position should be used first (so all master priorites should be the same)
-- then the shelf position as as tie breaker.
type SlotPriority = ( Int    -- ^ master priority
                    , Double -- ^ position
                    , Int    -- ^ tie breaker
                    )

  
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
combineSlicess :: forall s p . [(ExitMode, [(Shelf s, Slices Double p)])] -> Slices SlotPriority (Shelf s, p)
combineSlicess shelf'slicess = let
  -- assign a number for each shelf and then
  -- reuse the same number within a group if needed.
  -- This way 1 2 3 4 5 will become 1 2 3 4 5 
  -- if shelves 3 and 4 are need to be filled togother
  withN :: [ (ExitMode, [ ( ArgI (Shelf s)
                          , Slices Double p
                          )
                        ]
             )
           ]
  withN = flip evalState 1 do
               forM shelf'slicess \(exitMode, ss) ->  do
                 ssWithN <- forM ss \(shelf, slices) -> do
                         i <- get
                         modify (+1)
                         return (Arg i shelf, slices)
                 return $ (exitMode, ssWithN)
  sameStrategy :: [ (ExitMode, [ SimilarBy FillingStrategy
                               ( ArgI (Shelf s)
                               , Slices Double p
                               )
                               ]
                    )
                  ]
  sameStrategy = map (fmap $ groupSimilar (shelfFillingStrategy . argued . fst)) withN
  withGroupN = concat [ map (adjustN exitMode) same
                      | (exitMode, same) <- sameStrategy
                      ]
  adjustN :: ExitMode -> SimilarBy FillingStrategy
                         ( ArgI (Shelf s) -- shelf with initial position 
                         , Slices Double p -- slices indexed by 
                         )
          -> [ Slices SlotPriority (Shelf s, p)
             ]
  adjustN exitMode (SimilarBy strategy s'is1 s'i'slices) = 
    let firstI = argument (fst s'is1)
        adjust i d = if (exitMode, strategy) `elem` [(ExitOnTop, ColumnFirst), (ExitLeft, RowFirst)]
                   then (firstI, d, i) -- sames "group" , within a group fill slice then go to the next shelf
                   else (i, d, i) -- fill shelf first, then go to the next one
    in [ bimap (adjust i) (shelf,) slices
       | (Arg i shelf, slices) <- s'is1 : s'i'slices
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

data SlotBounds = SlotBounds { lowerB, upperB :: Maybe Text }
    deriving (Show, Eq, Ord)
    
inBound :: SlotBounds -> Text -> Bool
inBound SlotBounds{..} x =
     (maybe (const True) (<=) lowerB ) x && 
     (maybe (const True) (>=) upperB) x

-- | make group of pair slots boxes so that boxes are "authorized" to be in the correct slots
   -- we assume slots are sorted "correctly".
   -- Therefore, group of empty slots corresponds to
   -- sequence of Positions, delimiter by overlap boxes.
   -- Those boxes give the lower and upper bound of the group.
   -- For  example given (number are empty slot/pos and letter are boxes
   -- 
   --  1 2 3 [A B] 5 6 [M] [P] 7
   -- this will give 3 groups of avaible slots
   --     1 2 3 : =< A
   --     4 6 : >= B & =< M
   --     7   : >= 7
forSortedOverlap :: forall k s . Ord k => Slices k (Shelf s, BoxesOrPos s) -> (SimilarBoxes s) -> [(Slices k (Shelf s, Position), (SimilarBoxes s))]
forSortedOverlap positionsWithShelf allBoxes = let
  style = snd $ similarKey allBoxes
  bounded = addSlotBounds (boxContentFor style) positionsWithShelf
  groupedByBound :: [(SlotBounds, Slices k (Shelf s, Position))]
  groupedByBound = [ (bound, slicesOfPos)
                   | (bound, slicesWithBound) <- groupSlicesWithKey fst bounded
                   , let slicesOfShelfAndBoxOrPos = fmap snd slicesWithBound
                         (_,slicesOfPos) = partitionEitherSlices $ map (\(s, e) -> fmap (s,) e) slicesOfShelfAndBoxOrPos 
                   ]

  (leftOverM, slicesWithBoxes) = List.mapAccumL go (Just allBoxes) groupedByBound
  in catMaybes slicesWithBoxes <> case leftOverM of
                                     Nothing -> []
                                     Just leftOver -> [(mempty, leftOver)]
  where go Nothing _ = (Nothing, Nothing)
        go (Just boxes) (bound, slices) = let
           (inbound, outofbound) = partitionSimilar (inBound bound . boxContent) boxes
           in case inbound of
                     Nothing -> (Just boxes, Nothing)
                     Just ins -> let availableNumber = F.length slices
                                     -- boxes needs to be allocated in content order, but
                                     -- to get the correct number for each content 
                                     -- we need to find the boxes fitting in the group
                                     -- and then sort them by content
                                     (,) toFitM leftOverM  = splitSimilar availableNumber ins 
                                     -- we need to add the unused boxes at the end so they are not "lost"
                                     -- and get excluded if needed
                                     toFitSorted = sortSimilarOn boxContent <$> toFitM
                                     
                                 in ( unsplitM leftOverM outofbound
                                    , (slices,) <$> toFitSorted
                                    )
        unsplitM ma mb= case (ma, mb) of
                   (Just a, Just b) -> unsplitSimilar a b
                   _ -> ma <|> mb
        boxContentFor style box = if style == boxStyle box
                                  then Just $ boxContent box
                                  else Nothing
            
addSlotBounds :: forall b s p slices_k . Traversable slices_k =>  (b -> Maybe Text) -> slices_k (s, Either (NonEmpty b) p) -> slices_k (SlotBounds , (s, Either (NonEmpty b) p))
addSlotBounds f slices = let
  startBound = SlotBounds Nothing Nothing
  withLowerBound :: slices_k (SlotBounds, (s, Either (NonEmpty b) p))
  withLowerBound = snd $ List.mapAccumL accLowerBound startBound slices
  in snd $ List.mapAccumR accUpperBound startBound withLowerBound
  where accLowerBound lastBound s@(_,posOrBoxes) =
            let newBound = case posOrBoxes of
                                       Left boxes -> case getContents boxes of
                                                       Nothing -> lastBound
                                                       Just contents -> let newLower = F.maximum  contents
                                                                        in SlotBounds (Just newLower) Nothing
                                       Right _ -> lastBound
                in (newBound, (newBound, s))
        accUpperBound lastBound (bound, s@(_,posOrBoxes)) =
            let newBound = case posOrBoxes of
                                       Left boxes -> case getContents boxes of
                                                       Nothing -> lastBound
                                                       Just contents -> let newUpper = F.minimum  contents
                                                                        in bound { upperB = Just newUpper}
                                       Right _ -> bound { upperB = upperB lastBound }
            in (newBound, (newBound, s))
        getContents :: NonEmpty b -> Maybe (NonEmpty Text)
        getContents = NE.nonEmpty . mapMaybe f . toList
                                                  
                                                    

                                                       
                          
-- | Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
{- rST::priority

Boxes are selected in semi-arbitrary order which can be modified
setting up priority. The order in which box are selected affect the
way boxes are actually stacked on shelves but also which boxese are
selected when using number restriction (see above). By default boxes
are selected in order by

-  global priority
-  style name (ascending)
-  style priority (priority within style)
-  content name
-  content priority (priority within content)

By default, all priorities are set 100. Priorities can be modified by
assigning the special tags, ``@global``, ``@style``, ``@content`` but
also any other tag using the ``^[tag]`` notation (see number
restriction).

-  ``@global`` global priority. Can be used to move a box first.
-  ``@style`` priority within the same style. Can be use to move a
   box at the beginning of a style.
-  ``@content`` priority within box of the same content (styles and
   variations). Can be

For example, given fox boxes, A-Black, A-Red, B-Black, B-Red. Boxes
will be stacked in the following fs order

-  B-Black
-  B-Red
-  A-Black
-  A-Red

or

-  A-Black
-  A-Red
-  B-Black
-  B-Red

A and B having the same global priority, the system is free to start
with A or B. However, content (Black and Red) are sorted
alphabetically. To ensure that, A is processed before B. We need to
assign it a priority < 100 to A (global priority) with

::

   A,@content=1

To get B-Red boxes before B-Black boxes we can assign it a priority
(style priority)

::

   B-Red,@style=1

Settings those two priorities will result in the following order :

-  A-Black # @style=100 @content=1
-  A-Red # @style=100 @content=1
-  B-Red # @style=1 @content=100
-  B-Black # @style=100 @content=100

The content priority could be used for example, to select which one
of the B-Black boxes to get first.
::rST -}
moveBoxes :: (Box' box , Shelf' shelf) => PartitionMode -> SortBoxes -> [(box s)] -> [(ExitMode, [shelf s])] -> WH (InExcluded (Box s)) s
moveBoxes partitionMode sortMode bs exit'ss = do
  boxes <- mapM findBox bs
  let sorted = (if sortMode == SortBoxes then sortOnIf boxGlobalRank else id)
               $ boxes
      boxGlobalRank box = (boxGlobalPriority box, boxStyle box, boxStylePriority box,  _boxDim box, boxContent box, boxContentPriority box)
      -- \^ we need to regroup box by style and size
      -- However we take into the account priority within the style before the dimension
      -- so that we can set the priority
  inEx <- moveSortedBoxes partitionMode (map (,()) sorted) exit'ss
  return $ fmap fst inEx


moveSortedBoxes :: (Box' box , Shelf' shelf) => PartitionMode -> [(box s, p)] -> [(ExitMode, [shelf s])] -> WH (InExcluded (Box s, p)) s
moveSortedBoxes partitionMode bs exit'ss = do
  boxes <- mapM (firstM findBox) bs
  let layers = groupBy ((==)  `on` boxBreak . fst ) boxes
      boxBreak box = (boxStyle box, _boxDim box)
  (unzip -> (ins, exs))  <- forM layers $ \layer -> do
    let groups = groupSimilar (\(b,_) -> (_boxDim b, boxStyle b)) layer
    moved'lefts <- mapM (\g -> do
                            let (boxes, priorities) = unzipSimilar g
                            moved'lefts <- moveSimilarBoxes partitionMode boxes exit'ss
                            return case moved'lefts of
                                     (Nothing, Just _) -> (Nothing, Just g)
                                     (Just _ , Nothing)  -> (Just g, Nothing)
                                     (Just moved, Just lefts) ->
                                         -- boxes have been moved in order, moved + left == boxes
                                         -- we can reassign the priorities in order
                                         let (Just pmoved, Just pleft) = splitSimilar (length $ unSimilar moved) priorities
                                         in (zipSimilar moved pmoved, zipSimilar lefts pleft)
                                     (Nothing, Nothing) -> (Nothing, Nothing)
                        ) groups
    let (moveds, lefts) = unzip moved'lefts
    return (concatMap unSimilar $ catMaybes moveds, concatMap unSimilar $ catMaybes lefts)
  return $ InExcluded (Just $ concat ins) (Just $ concat exs)

moveAndTag :: ExContext s -> [Text] -> (BoxSelector, [Text], Maybe Text, [OrientationStrategy]) -> WH (InExcluded (Box s, Priority))  s
moveAndTag ec tagsAndPatterns_ (style, tags_, locationM, orientations) = withBoxOrientations orientations $ do
  newBaseEvent "TAM" $ intercalate "," [ intercalate "#" (printBoxSelector style :  tags_)
                                 , fromMaybe "<tag only>" locationM
                                 , mconcat $ map tshow orientations
                                 ]
  let withNoEmpty = partition ((== "@noempty") . toLower)
      (noEmpty1, tagsAndPatterns) = withNoEmpty tagsAndPatterns_
      (noEmpty2, tags) = withNoEmpty tags_
      noEmpty = not . null $ noEmpty1 <> noEmpty2

      -- don't resort boxes if a number selector has been set.
      sortMode = case numberSelector style  of
                      BoxNumberSelector NoLimit NoLimit NoLimit -> SortBoxes
                      _ -> DontSortBoxes
      tagOps = parseTagAndPatterns tagsAndPatterns tags
  box'prioritys <- narrowBoxes style ec >>= getBoxPs-- findBoxByNameAndShelfNames style
  case (box'prioritys, noEmpty) of
       ([], True) -> error $ show style ++ " returns an empty set"
       _         -> return ()
  inEx <- case locationM of
              Just location' -> do
                   -- reuse leftover of previous locations between " " same syntax as Layout
                   moveToLocations ec sortMode box'prioritys location'
                   -- foldM (\boxInEx locations -> do
                   --            let (location, (exitMode, partitionMode, addOldBoxes, sortModeM)) = extractModes locations
                   --            let locationss = splitOn "|" location
                   --            shelves <- findShelfBySelectors (map parseSelector locationss)
                   --            ie <- aroundArrangement addOldBoxes (moveBoxes exitMode partitionMode $ fromMaybe sortMode sortModeM) (excludedList boxInEx) shelves
                   --            return $ ie { included = Just $ includedList boxInEx ++ includedList ie }
                   --       ) (mempty { excluded = Just boxes}) (splitOn " " location')
              Nothing -> return $ mempty { included = Just box'prioritys } 
  case tags of
    [] -> return inEx
    _  -> do
      let untagOps = negateTagOperations tagOps
      newIn <- zipWithM (\b i -> firstM (flip (updateBoxTags tagOps) i) b ) (includedList inEx) [1..]
      newEx <- zipWithM (\b i -> firstM (flip (updateBoxTags untagOps) i) b) (excludedList inEx) [1..]
      return $ InExcluded (Just newIn) (Just newEx)

-- with each group having "mode" and locations
-- A|B ^C|D means try A or B, then C or D with exit on top mode
moveToLocations :: Ord p => ExContext s -> SortBoxes -> [(Box s, p)] -> Text -> WH (InExcluded (Box s, p)) s
moveToLocations ec sortMode boxes location = do
   foldM (\boxInEx locations -> do
             let (location, (exitMode, partitionMode, addOldBoxes, sortModeM)) = extractModes locations
             let locationss = splitOnNonEscaped "|" location
             shelves_ <- findShelfBySelectors (map parseSelector locationss)
             let shelves = filter inEC shelves_
                 inEC s = case included (ecShelves ec) of
                             Nothing -> True
                             Just sid ->  s `elem` sid
             ie <- aroundArrangementWithP addOldBoxes
                                     ( \bs ss -> moveBoxes 
                                                 (fromMaybe (ecPartitionMode ec) partitionMode)
                                                 ( fromMaybe sortMode sortModeM)
                                                 bs
                                                 [(exitMode, ss)]
                                     )
                                     (excludedList boxInEx) shelves
             return $ ie { included = Just $ includedList boxInEx ++ includedList ie }
        ) (mempty { excluded = Just boxes}) (splitOnNonEscaped " " location)

splitTagsAndLocation :: Text -> ([Text], Maybe Text)
splitTagsAndLocation tag'locations
   -- -| (tag, _:location@(_:_)) <- break (=='/') tag'locations = (just tag, just location)
   | (location , uncons -> Just (_,tag@(uncons -> Just _))) <- break (=='#') tag'locations = (splitOnNonEscaped "#" tag, just location)
   | otherwise = ([], Just tag'locations)
   where just "" = Nothing
         just s = Just s
    
