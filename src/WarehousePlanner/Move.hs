{-# LANGUAGE ScopedTypeVariables #-}
module WarehousePlanner.Move
( aroundArrangement 
, bestArrangement
, bestPositions, bestPositions'
, cornerHull
, stairsFromCorners
, moveBoxes
-- , moveSimilarBoxes
, moveAndTag
, moveToLocations
, splitTagsAndLocation
-- reexport
, withAll
)
where 
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import qualified Prelude
import Control.Monad.State(gets)
import Control.Monad hiding(mapM_,foldM)
import Data.List.NonEmpty(unzip)
import Data.List qualified as List
import WarehousePlanner.Type
import WarehousePlanner.Slices
import WarehousePlanner.SimilarBy
import WarehousePlanner.Base
import WarehousePlanner.Selector
import WarehousePlanner.Affine
import WarehousePlanner.Tiling
import WarehousePlanner.WPL.ExContext
import Data.Text(splitOn, uncons)
import Data.Foldable qualified as F
import Data.Set qualified as Set

-- | Remove boxes for shelves and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement :: (Shelf' shelf, Box' box, Box' box2)
                  => forall s . AddOldBoxes 
                  -> (forall b . Box' b =>  [b s] -> [shelf s] -> WH (InExcluded (box2 s)) s)
                -> [box s] -> [shelf s] -> WH (InExcluded (box2 s)) s
aroundArrangement useOld arrangement newBoxishs shelves = do
    newBoxes <- mapM findBox newBoxishs
    let -- newSet :: Set (Box s)
        newSet =  Set.fromList newBoxes 
    boxesBefore <- case useOld of
              NewBoxesOnly -> return newBoxes
              AddOldBoxes -> do
                          allOldBoxes <- concatMap reverse `fmap` mapM findBoxByShelf shelves
                          -- make sure news and old boxes don't have boxes in commun
                          let oldBoxes = filter (`notMember` newSet) allOldBoxes
                          return $ oldBoxes ++ newBoxes

    let nothing = Nothing `asTypeOf` headMay shelves -- trick to typecheck
    boxes <-  mapM (assignShelf nothing) boxesBefore
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
type BoxesOrPos b = Either [b] Position


-- | Find the  best box positions for similar boxes and a given shelf.
-- This takes into account the boxes already present in the shelf and
-- the possible orientation and shelf strategy.
bestPositions :: PartitionMode -> Shelf s -> SimilarBoxes s -> WH (Slices Double Position) s
bestPositions partitionMode shelf simBoxes = do
  bOrPs <- bestPositionOrBoxes partitionMode shelf simBoxes
  return . snd $ partitionEitherSlices bOrPs

bestPositionOrBoxes :: PartitionMode -> Shelf s -> SimilarBoxes s -> WH (Slices Double (BoxesOrPos (Box s))) s
bestPositionOrBoxes partitionMode shelf simBoxes = do
  let SimilarBy dim box _ = simBoxes
  boxesInShelf <- findBoxByShelf shelf
  boxo <- gets boxOrientations
  let orientations = boxo box shelf
  return $ bestPositions' boxAffDimension partitionMode orientations shelf mempty boxesInShelf dim 

bestPositions' :: forall b s . (b -> AffDimension ) -> PartitionMode -> [OrientationStrategy] -> Shelf s -> Dimension -> [b] -> Dimension -> Slices Double (BoxesOrPos b)
bestPositions' getAff pmode orientations shelf start used dim | isOverlap pmode = let
   -- try each orientation strategy individually as if the box was empty
   -- and remove the "used" positions. Then get the best one
  solutions = [ fmap isUsed $ justifyPositions $ bestPositions' getAff PRightOnly [strategy]  shelf start [] dim 
              | strategy <- orientations
              ]
  sorted = sortOn (Down . Prelude.length . snd . partitionEitherSlices) solutions
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
           in case overlappings of
                [] -> Right pos
                _ -> Left overlappings
        isUsed left = left
        isOverlap = \case 
                     POverlap _ -> True
                     PSortedOverlap -> True
                     _ -> False
        justifyPositions = case pmode of
                             POverlap ORight -> justifyRight dim (minDim shelf)
                             POverlap OAligned -> justifyAlign (map getAff used) (minDim shelf)
                             _ -> id
                             
                      

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
justifyRight :: Dimension -> Dimension -> Slices Double (BoxesOrPos b) -> Slices Double (BoxesOrPos b)
justifyRight box shelf slices = let
   (_,poss) = partitionEitherSlices slices
   pos = F.toList poss
   in case pos of
           [] -> mempty
           _ -> let maxL = maximumEx (map (dLength . aTopRight . positionToAffine box) pos)
                    offset = Dimension (max 0 $ dLength shelf - maxL) 0 0 
                in  fmap (fmap (\Position{..} -> Position {pOffset=pOffset <> offset,..})) slices
-- | Offset positions to align with the most right start of a box
justifyAlign :: [AffDimension] -> Dimension -> Slices Double (BoxesOrPos b) -> Slices Double (BoxesOrPos b)
justifyAlign used shelf slices = let
   (_,poss) = partitionEitherSlices slices
   pos = F.toList poss
   in case used of
      [] -> slices
      _ -> let maxL = maximumEx $ map (dLength . aBottomLeft) used
               -- using maxL as offset doesn't allow to fit box on hole before maxL
               -- to be able to use them we need to only offset  bit.
               -- The whole grid can only be shifted by an amount lesser than the difference
               -- between the used length and the shelf length (the extra space on the right).
               -- We use that to do a module
               spaceLeft = dLength shelf - maximumEx (map (dLength . pOffset) pos)
               l = if maxL <= spaceLeft || spaceLeft == 0
                   then maxL
                   else let q = maxL / spaceLeft
                        in maxL - (fromIntegral (floor q) * spaceLeft)
               offset = Dimension l 0 0
           in fmap (fmap (\Position{..} -> Position {pOffset=pOffset <> offset,..})) slices
           
   
 
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
moveSimilarBoxes :: (Shelf' shelf) => ExitMode -> PartitionMode -> SimilarBoxes s -> [shelf s] -> WH (Maybe (SimilarBoxes s), Maybe (SimilarBoxes s)) s
moveSimilarBoxes exitMode partitionMode boxes shelves' = do
  shelves <- mapM findShelf shelves'
  pos'boxs <- mapM (\s -> bestPositionOrBoxes partitionMode s boxes) shelves
  let positionsWithShelf = combineSlices exitMode $ zip shelves pos'boxs
      posWithShelf'boxesz = case partitionMode of
         PSortedOverlap -> forSortedOverlap positionsWithShelf boxes
         _              -> [(snd $ partitionEitherSlices $ fmap sequenceA positionsWithShelf, boxes)]
  toUnzip <- mapM (\(pws, bs) ->  assignBoxesToPositions pws bs) posWithShelf'boxesz
  let (lefts, rights) = unzip toUnzip
      tomaybe xms = case catMaybes xms of
                      [] -> Nothing 
                      (x:xs) -> foldM (\m s -> unsplitSimilar m s) x xs
  return (tomaybe lefts, tomaybe rights)
  
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
combineSlices :: ExitMode -> [(Shelf s, Slices Double p)] -> Slices (Int, Double, Int) (Shelf s, p)
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

-- | make group of pair slots boxes so that boxes are "authorized" to be in the correct slots
forSortedOverlap :: Slices k (Shelf s, BoxesOrPos (Box s)) -> (SimilarBoxes s) -> [(Slices k (Shelf s, Position), (SimilarBoxes s))]
forSortedOverlap positionsWithShelf boxes =
         [(snd $ partitionEitherSlices $ fmap sequenceA positionsWithShelf, boxes)]
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

moveAndTag :: ExContext s -> [Text] -> (BoxSelector, [Text], Maybe Text, [OrientationStrategy]) -> WH (InExcluded (Box s))  s
moveAndTag ec tagsAndPatterns_ (style, tags_, locationM, orientations) = withBoxOrientations orientations $ do
  newBaseEvent "TAM" $ intercalate "," [ printBoxSelector style
                                 , intercalate "#" tags_
                                 , fromMaybe "" locationM
                                 , mconcat $ map tshow orientations
                                 ]
  let withNoEmpty = partition (== "@noEmpty")
      (noEmpty1, tagsAndPatterns) = withNoEmpty tagsAndPatterns_
      (noEmpty2, tags) = withNoEmpty tags_
      noEmpty = not . null $ noEmpty1 <> noEmpty2

      -- don't resort boxes if a number selector has been set.
      sortMode = case numberSelector style  of
                      BoxNumberSelector NoLimit NoLimit NoLimit -> SortBoxes
                      _ -> DontSortBoxes
      tagOps = parseTagAndPatterns tagsAndPatterns tags
  boxes0 <- narrowBoxes style ec >>= getBoxes-- findBoxByNameAndShelfNames style
  case (boxes0, noEmpty) of
       ([], True) -> error $ show style ++ " returns an empty set"
       _         -> return ()
  boxes <- mapM findBox boxes0
  inEx <- case locationM of
              Just location' -> do
                   -- reuse leftover of previous locations between " " same syntax as Layout
                   moveToLocations ec sortMode boxes location'
                   -- foldM (\boxInEx locations -> do
                   --            let (location, (exitMode, partitionMode, addOldBoxes, sortModeM)) = extractModes locations
                   --            let locationss = splitOn "|" location
                   --            shelves <- findShelfBySelectors (map parseSelector locationss)
                   --            ie <- aroundArrangement addOldBoxes (moveBoxes exitMode partitionMode $ fromMaybe sortMode sortModeM) (excludedList boxInEx) shelves
                   --            return $ ie { included = Just $ includedList boxInEx ++ includedList ie }
                   --       ) (mempty { excluded = Just boxes}) (splitOn " " location')
              Nothing -> return $ mempty { included = Just boxes } 
  case tags of
    [] -> return inEx
    _  -> do
      let untagOps = negateTagOperations tagOps
      newIn <- zipWithM (updateBoxTags tagOps) (includedList inEx) [1..]
      newEx <- zipWithM (updateBoxTags untagOps) (excludedList inEx) [1..]
      return $ InExcluded (Just newIn) (Just newEx)

-- with each group having "mode" and locations
-- A|B ^C|D means try A or B, then C or D with exit on top mode
moveToLocations ec sortMode boxes location = do
   foldM (\boxInEx locations -> do
             let (location, (exitMode, partitionMode, addOldBoxes, sortModeM)) = extractModes locations
             let locationss = splitOn "|" location
             shelves_ <- findShelfBySelectors (map parseSelector locationss)
             let shelves = filter inEC shelves_
                 inEC s = case included (ecShelves ec) of
                             Nothing -> True
                             Just sid ->  s `elem` sid
             ie <- aroundArrangement addOldBoxes (moveBoxes exitMode partitionMode $ fromMaybe sortMode sortModeM) (excludedList boxInEx) shelves
             return $ ie { included = Just $ includedList boxInEx ++ includedList ie }
        ) (mempty { excluded = Just boxes}) (splitOn " " location)

splitTagsAndLocation :: Text -> ([Text], Maybe Text)
splitTagsAndLocation tag'locations
   -- -| (tag, _:location@(_:_)) <- break (=='/') tag'locations = (just tag, just location)
   | (location , uncons -> Just (_,tag@(uncons -> Just _))) <- break (=='#') tag'locations = (splitOn "#" tag, just location)
   | otherwise = ([], Just tag'locations)
   where just "" = Nothing
         just s = Just s
    
