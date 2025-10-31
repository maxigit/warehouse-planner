-- | Miscelaneous reporting functions
module WarehousePlanner.Report
( reportAll
, report
, summary
, expandReportValue
, generateMoves
, generateMOPLocations
, generateGenericReport
, bestBoxesFor
, bestFitReport
, tagShelvesWithFitData
, bestShelvesFor
, bestAvailableShelvesFor
, bestHeightForShelf
, shelvesReport
, shelvesReportFor
, shelfTagsReport
, boxesReport
, groupShelvesReport
, groupShelvesReport'
, groupBoxesReport
, reportPairs
, boxStyleWithTags
, generateStockTakes
, generateFuzzyStockTakes
, generateBoxesHistory
, boxHistory
, generateTags
) where

import WarehousePlanner.Base
import WarehousePlanner.Affine (boxAffDimension, AffDimension(..), positionToAffine)
import WarehousePlanner.Optimum
import WarehousePlanner.Summary
import WarehousePlanner.History
import WarehousePlanner.Move
import WarehousePlanner.Selector
import WarehousePlanner.Tiling
import ClassyPrelude hiding(or)
import Control.Monad.ST.Unsafe(unsafeSTToIO)
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.State(get, gets, evalStateT)
import Text.Printf(printf)
import Data.Map.Strict qualified as Map'
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Sequence qualified as Seq
import Data.List qualified as List
import Text.Tabular as Tabul
import Data.Text(replace, breakOn)
import Data.List.NonEmpty (NonEmpty(..))
import WarehousePlanner.SimilarBy
import Data.Foldable qualified as F

-- pattern (:<) :: Text -> Maybe (Char, Text)
pattern x :< xs <- (uncons -> Just (x, xs))
reportAll :: WH [Text] s
reportAll = do
    sb <- shelfBoxes
    let groups =  Map'.fromListWith (+) (map (\(s,b) -> ((shelfName s, boxDim b, orientation b, boxStyle b), (1::Int))) sb)
    return $ map toString (Map'.toList groups)
    where toString ((name, __dim, o, style), count) =
            name
                <> ", " <> style
                <> ", " <> tshow count  <> ", " <> showOrientation o
{- rST::bests

.. _best-reports:

Best boxes, best shelves and best available shelves computes
respectivily the best boxes to go in a given shelf, the best shelves to
hold the given boxes. The boxes/shelves to analyses should be set in the
parameter field. If the field starts with ``!`` then depth of shelf and
boxes is not used in displaying the used ratio. Fol best shelf reports,
a set of boxes and shelves can be given using the
``boxSelector[,shelfSelector[,orientationRules]]`` syntax. In that case,
only the given boxes and shelves will be taken into account for the
report.
::rST -}
-- | Find the box which fit a given shelf  the best
bestBoxesFor :: Text -> WH [Text] s
bestBoxesFor (extractRanking -> (ranking, shelf)) = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfBySelector (Selector (matchName shelf) []) >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes
    let s  = headEx shelves
        tries = [ (-(usedRatio ranking box {orientation = or} s tilingMode), box)
            | (box, ors) <- bors
            , let (or, tilingMode,_) = bestArrangement (ors s) [(minDim s, maxDim s, s)] (_boxDim box)
            ]

        bests = sortOn fst tries

    mapM (report ranking s) (map snd bests)

-- | Find best boxes ignoring the shelve height
bestHeightForShelf :: Text -> WH (IO ()) s
bestHeightForShelf shelf = do
    boxIds <- toList <$> gets boxes
    boxes' <- mapM findBox boxIds
    shelves <- findShelfBySelector (Selector (matchName shelf) []) >>= mapM findShelf
    getOr <- gets boxOrientations

    let groups  = Map'.fromList (map (\b -> ((boxDim b, boxStyle b), b)) boxes')
        boxes = Map'.elems groups
    let bors = map (\b -> (b, getOr b)) boxes

    let s  = headEx shelves
        tries = [ (-((fromIntegral $ tmTotal tilingMode)*boxVolume box / shelfVolume s), (box, or, tilingMode))
                | (box, ors) <- bors
                , let (or,tilingMode,_) = bestArrangement (ors s) [(minDim s, maxDim s, s)] (_boxDim box)
                ]

        bests = sortOn fst tries
        reportHeight shelf_  (box, or, tilingMode) = do
          similarBoxes <- findBoxByNameSelector (matchName $ boxStyle box)
          let box' = box { orientation = or }
              n = tmLength tilingMode
              k = tmDepth tilingMode
          let lengthRatio = ( dLength (boxDim box') * (fromIntegral n)
                            / dLength (minDim shelf_)
                            )
              widthRatio = ( dWidth (boxDim box') * (fromIntegral k)
                           / dWidth (minDim shelf_)
                           )
              height = dHeight (boxDim box')
              recommendedHeight = height * (fromIntegral ((ceiling (130/height)) :: Int))
              numberOfBoxes = length similarBoxes
              shelvesNeeded = fromIntegral numberOfBoxes * recommendedHeight / height / fromIntegral n

          return $ putStrLn $ "box: " <> boxStyle box
            <> ", height: " <> (pack $ printf " %0.2f (%0.2f)" recommendedHeight height)
            <> " " <> showOrientationWithDiag or tilingMode
            <> (pack $ printf " %dx%dx%d" n k (floor (recommendedHeight/height) :: Int))
            <> (pack $ printf " H:%0.1f%%" (100*lengthRatio))
            <> (pack $ printf " (%0.1f%%)" (100*lengthRatio*widthRatio))
            <> (pack $ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double))

    ios <- mapM (reportHeight s) (map snd bests)
    return $ sequence_ ios

report :: Ranking -> Shelf s -> Box s -> WH Text s
report ranking shelf box = do
    getOr <- gets boxOrientations
    similarBoxes <- findBoxByNameSelector (matchName $ boxStyle box)
    let (or,tilingMode,_) = bestArrangement (getOr box shelf) [(minDim shelf, maxDim shelf, shelf)] (_boxDim box)
        ratio = usedRatio ranking box {orientation = or} shelf tilingMode
        numberOfBoxes = length similarBoxes
        shelvesNeeded = fromIntegral numberOfBoxes / fromIntegral (tmTotal tilingMode)
    return $ "box: " <> boxStyle box <> ", shelf: " <> shelfName shelf
                        <> " " <> showOrientationWithDiag or tilingMode
                        <> (pack $ printf " %0.1f%%" (ratio*100))
                        <> " " <> (showHowManysFor tilingMode)
                        <> (pack $ printf " (%d)" (tmTotal tilingMode))
                        <> (pack $ printf " : %d b -> %0.1f s " numberOfBoxes (shelvesNeeded :: Double))
  
                        -- <> tshow (_boxDim box) <> tshow (maxDim shelf)



extractRanking :: Text -> (Ranking, Text)
extractRanking s = case uncons s of
                        Just ('!', after) -> (Rank2D, after)
                        _ -> (Rank3D, s)
-- | find the best shelf for a given style
-- Doesn't take into account boxes already there.
bestShelvesFor :: Text -> WH [Text]s
bestShelvesFor (extractRanking -> (ranking, style'shelf)) = do
    (boxes, shelves, or) <- boxAndShelvesFor style'shelf

    let box = headEx boxes
        bests = bestShelves ranking box (or box) shelves

    mapM (\shelf ->  report ranking shelf box) bests

boxAndShelvesFor :: Text -> WH ([Box s], [Shelf s], Box s -> Shelf s -> [OrientationStrategy]) s
boxAndShelvesFor style'shelf'ors = do
  ors0 <- gets boxOrientations
  let (style, shelfSelector, ors) = case (splitOnNonEscaped "," style'shelf'ors) of
                                  [box, shelf] -> (box, shelf, ors0)
                                  [box, shelf, rules] -> (box, shelf, \_ _ -> parseOrientationRule [] rules)
                                  _ -> (style'shelf'ors, "", ors0)
                                  
  boxes <- findBoxByNameSelector (matchName $ style) >>= mapM findBox
  shelves <- findShelfBySelector (parseSelector shelfSelector) >>= mapM findShelf
  return (boxes, shelves, ors)

-- | find the best shelf for a given style
-- depends on what's already there.
bestAvailableShelvesFor :: PartitionMode ->Text -> WH [Text] s
bestAvailableShelvesFor pmode (extractRanking -> (ranking, style'shelf)) = do
    (boxes, shelves, or) <- boxAndShelvesFor style'shelf
    let  box = headEx boxes
    -- sort is stable so by passing shelves in
    -- the best order we get at shelves sorted by
    -- n and then best order
    let bests = bestShelves ranking box (or box) shelves
    let   getInfo shelf = do
            inEx <- moveBoxes pmode SortBoxes boxes [(ExitLeft, [shelf])]
            return (shelf, length (includedList inEx))
    shelfInfos <- mapM getInfo bests
    let go (shelf, n) = do
            r <- report ranking shelf  (headEx boxes)
            return $  (pack $ printf "%d/%d => " n (length boxes)) <> r
    mapM go ( sortOn (Down . snd)
                     (filter ((/=0).snd) shelfInfos)
                   )

{- rST::fit

.. _best-fit:

The **best fit** report is similar to the :ref:`best reports <best-reports>` but instead of giving the best shelves or best boxes
it gives all the possibilities for the selected shelves and boxes. Best shelves or best boxes for can be acheived by selecting all shelves or all boxes.
Different columns gives or rows gives results for used or empty shelves, pickable or all boxes etc.

The best shelf for a given box is done by selecting one 

Shelf mode
``````````
For each (shelf, box) combination a row is produce for different *shelf mode* that is, ``full``, ``right`` or ``above``.

- ``full``: fit as if the shelf was empty (uses the full shelf regardless of what is already in )
- ``right``: fit using the empty part of the shelf at the right of existing boxes. Similar to using ``right`` partition mode.
- ``above``: fit using the empty part of the shelf above the of existing boxes. Similar to using ``above`` partition mode.

Choosing all the available choices in the same report seems to be easier to use than having to chose one before launching the report.

Pickable
````````
The best fit report can distinguished between *pickable* and *not pickable* boxes. Pickable are box which can be picked from
without having to move other boxes. They are the boxes with ``w=1``. 
For example, if a layout allow to fit 8 boxes  as 2x2x2 cube  in a shelf. Only 4 (2x2) will be pickable.

Boxes that we want to be in *pickable* zone can be marked with the tag ``@topick``.
 
In the report ``to_pick`` is the number of box we want to be pickable (``@topick``), whereas ``pickable`` is the number of boxes that can be picked from the given layout.
``picking_shelves`` is the number of identical shelves required to make all box to pick as pickable.

Required vs Fitted
``````````````````
Depending on the ``limitToBoxNb`` parameter the report will try to fit either as much boxes as possible (**fitted**) or just the number of box selected (**to fit**).
**required* or field starting with **r** refers to either the fitted or to fit boxes.

Field description
`````````````````

- ``shelf``:  the shelf name
- ``content``:  styles of boxes in the shelf
- ``part``: the shelf/partition mode
- ``style``:  the style being fit
- ``box``: box dimension
- ``fit``: how many can fit (according to part mode)
- ``to_fit``:  how many boxes are selected
- ``to_pick``:  how many boxes are ``@topick``
- ``pickable``: how many boxes can be picked easyly
- ``l100``:  required length / available length
- ``w100``:  required witd / available width
- ``h100``:  required heigh / available height
- ``wh100``:  
- ``lh100``: 
- ``lw100``: 
- ``fit100``:  fitted / to fit
- ``shelves_needed``:  how many shelves to fit all required
- ``picking_shelves``: how many shelves to have enough pickable
- ``rvolmin100``: required volume / available volume
- ``rvolmax100``:  required volume / available max volume (use  max shelf dimension - used)
- ``fvolmin100``:  fitted volume / available volume
- ``fvolmax100``:  fitted volume / availabe max volume
- ``orientation``: 
- ``how``: 
- ``fVSused``: fitted (=/= not to_fit) volume / used volume (by existing boxes)
- ``required``: required dimensions
- ``used``: used dimensions (by exists boxes)
- ``leftover``: shelf - used - required
- ``minShelf``:  available shelf
- ``maxShelf``: availabe max shelf
- ``debug_strategy``:  show orientation strategy
- ``debug_tiling``: show tiling
- ``debug_volume``:  show volume

Practical use
`````````````

The best use of a shelf is either to maximize ``fvolmin100`` (fill the current shelf as best) or if there is enough left over to maximize ``hw100``. This guarantee the best use of the available length. Minimizing ``l100`` is not enough as it doesn't *reward* to use as much height and depth as possible.

``fVSused`` allows to check if replacing the current content of a shelf by all boxes of the given style is worth it.

``fit100`` allows to check if the present boxes will fill nicely in the available space.

Finding the best shelf for a style is done by selecting all shelves and only all boxes of a given style.
Finding the best box for a shelf by selecting all boxes and only the desired shelf.

Brick shortcuts
```````````````
In interactive mode, this report can be called with different combination of the current shelf, box, current selections or all.


    +-----------+-------------------------------------+--------+----------+------------+
    | Shortcut  |                                     | Limit  | Shelves  | Boxes      |
    +-----------+-------------------------------------+--------+----------+------------+
    | a         |  best fitted shelf                  | to fit | sel|All  | current    |
    +-----------+-------------------------------------+--------+----------+------------+
    | S         |  best shelf                         | All    | sel|All  | current    |
    +-----------+-------------------------------------+--------+----------+------------+
    | B         |  best fitted boxes                  | All    | current  | sel|All    |
    +-----------+-------------------------------------+--------+----------+------------+
    | f         |  best fitted selection              | All    | sel|cur  | sel|cur|All|
    +-----------+-------------------------------------+--------+----------+------------+
    | F         |  best selection                     | to fit | selected | sel|cur|All|
    +-----------+-------------------------------------+--------+----------+------------+


Instead of generating a text report, shelves can be tagged with the report information using ``ff`` or ``fF``.

The value of the tag(s) can be visualized for all shelves by setting the shelf property (using the ``f`` and ``F`` prefixes) to the desired tag and change the layout to display mode to display shelf properties.


 ::rST -}
 
bestFitReport :: forall s . Bool -> [Box s] -> [Shelf s] -> WH [Map Text Text] s
bestFitReport limitToBoxNb boxes shelves = do
   getOrs <- gets boxOrientations
   let groups  = groupSimilar (\b -> (_boxDim b, boxStyle b)) boxes
   let go :: SimilarBy (Dimension, Text) (Box s) -> Shelf s -> WH [Map Text Text] s
       go (SimilarBy (bdim,_) box bxs) shelf = do
           boxesInShelf <- findBoxByShelf shelf
           let toPick = length $ filter (flip tagIsPresent "@topick") bxs
           let ors = getOrs box shelf
               -- for empty shelf
               current@(Dimension lrequired _wrequired hrequired) = maxDimension $ map (aTopRight . boxAffDimension) boxesInShelf
               tries = [ ("full" , (minDim shelf, maxDim shelf))
                       , ("right", remaining shelf (Dimension lrequired 0 0))
                       , ("above", remaining shelf (Dimension 0 0 hrequired))
                       ]
           return . map (Map.fromList . addRank) $
                    [ [ ("shelf" :: Text, shelfName shelf)
                      , ("content", unwords [ t <> "x" <> tshow q | (t,q) <- Map.toList contents ])
                      , ("part", name)
                      , ("style", boxStyle box)
                      , ("box", printDim  bdim)
                      , ("fit",  pack $ printf "%3d"  fitted)
                      , ("to_fit", pack $ printf "%3d" toFit)
                      , ("to_pick", pack $ printf "%3d" toPick)
                      , ("pickable", tshow pickable)
                      , ("l100", percent requiredl sl) 
                      , ("w100", percent requiredw sw)
                      , ("h100", percent requiredh sh)
                      , ("wh100", percent (requiredw*requiredh) (sw*sh))
                      , ("lh100", percent (requiredl*requiredh) (sl*sh))
                      , ("lw100", percent (requiredl*requiredw) (sl*sw))
                      , ("fit100", percent (fi fitted) (fi toFit))
                      , ("shelves_needed", pack $ printf "%04.1f" (fi toFit / fi fitted))
                      , ("picking_shelves", pack $ printf "%04.1f" (fi toPick / fi pickable))
                      , ("rvolmin100", percent  (requiredl*requiredw*requiredh) (volume shelfMin))
                      , ("rvolmax100", percent  (requiredl*requiredw*requiredh) (volume shelfMax))
                      , ("fvolmin100", percent  (boxVolume box * fi fitted) (volume shelfMin))
                      , ("fvolmax100", percent  (boxVolume box * fi fitted) (volume shelfMax))
                      , ("orientation", showOrientationWithDiag or tilingMode)
                      , ("how", unwords [ pack $ printf "%dx%dx%d" (perLength hmany) (perDepth hmany) (perHeight hmany)
                                        | hmany <- toList $ tmHowManys tilingMode
                                        ]
                        )
                      , ("fVSused", case boxesInShelf of
                                          [] -> percent (1.0 :: Double) 1.0
                                          _ -> percent (boxVolume box * fi fitted) (volume current))
                      , ("required", printDim required)
                      , ("used", printDim current)
                      , ("leftover", printDim (shelfMin <> invert required))
                      , ("minShelf", printDim shelfMin)
                      , ("maxShelf", printDim shelfMax)
                      , ("debug_strategy", unwords $ map showOrientationStratety ors)
                      , ("debug_tiling", tshow tilingMode)
                      , ("debug_volume", pack $ printf "{\"required_volume\":%f, \"current\":%f, \"box_volume\":%f}"
                                                     (volume required)      (volume current) (boxVolume box)
                                                     )
                      ]
                    | (name, (shelfMin, shelfMax@(Dimension sl sw sh))) <- tries
                    , let (or, tilingMode,_) = bestArrangement ors [(shelfMin, shelfMax, ())] bdim
                    , let toFit = 1 + length bxs
                    , let fitted = tmTotal tilingMode
                    , let pickable = tmPickable tilingMode
                    , let required@(Dimension requiredl requiredw requiredh) =
                                maxDimension $ map (aTopRight . positionToAffine bdim )
                                             $ (if limitToBoxNb then (take toFit) else id)
                                             $ F.toList 
                                             $ generatePositions mempty ColumnFirst or (rotate or bdim) tilingMode
                    , fitted > 0
                    , let contents = Map.fromListWith (+) $ map (,1) $ map boxStyle boxesInShelf
                    ]
   concat <$> sequence [ go simBoxes shelf
                          | simBoxes <- groups
                          , shelf <- shelves
                          ]
  where addRank kvs = [ (pack (printf "%0.3d:" i) <> k, v)
                      | ((k, v), i) <- zip kvs  [1 :: Int ..]
                      ]
        remaining shelf required = let nrequired = invert required 
                               in (minDim shelf <> nrequired, maxDim shelf <> nrequired )
        percent a b = pack $ printf "%02.0f" (min 99 $ a*100/b)
        --                                   ^^^^^^^^^^^^^^^^^
        --                                   brick only shows the first number on
        --                                   shelf summary
        -- if we use %03 everything is nicely storted 001 010 099 etc
        -- but everything show as 0, hence using 2 char (and cap at 99)
        fi = fromIntegral @_ @Double
                     

-- | Tag all shelves with the result of fit report.
-- Tag keys are by default "style-tag"
tagShelvesWithFitData :: Maybe (Text -> Text -> Text) -> [Map Text  Text] -> WH () s
tagShelvesWithFitData adjustTagM rows = do 
   let adjustTag = fromMaybe (\style key -> style <> "-" <> key) adjustTagM
       keyKeys =  ["shelf", "style", "part"]
   forM_ rows  \fitMapWithRank -> do
       -- we need to remove the "rank" which is there to keep keys in initial order
       -- (vs alphabetical)
       let fitMap = Map.fromList [ (key, value)
                                 | (rank'key, value) <- Map.toList fitMapWithRank
                                 , let key = drop 1 $ snd $ breakOn ":" rank'key
                                 ]
       case traverse (flip lookup fitMap) keyKeys of
           Just [shelfname, style, partitionMode] ->  do
                shelves <- findShelfBySelector (Selector (matchName shelfname) []) >>= mapM findShelf
                forM_ shelves \shelf -> do
                    let tagOps = [ ( adjustTag style key , SetValues [value])
                                 | (tag, value) <- Map.toList fitMap
                                 , tag `notElem` keyKeys
                                 , "debug" `isPrefixOf` tag == False
                                 , let key = partitionMode <> "-" <> tag 
                                 ]
                    void $ updateShelfTags tagOps shelf
           _ -> error (show fitMap) -- return ()

-- | How many with depth = 1

tmPickable tm = sum [ perLength hm * perHeight hm |   hm <- toList $ tmHowManys tm ]

-- * Summary 
-- Display total volume shelf volume
-- with a breakdown per shelf tags

data SummaryInfo = SummaryInfo
  { siFloor :: Maybe Double --  ^ m^2
  , siTotalVolume :: Maybe Double -- ^ m^3
  , siUsedVolume :: Double -- ^ m3
  } deriving (Show)

siFree :: SummaryInfo -> Maybe Double
siFree si = do
  total <- siTotalVolume si
  return $ total - siUsedVolume si

siUsedPercent :: SummaryInfo -> Maybe Double
siUsedPercent si = do
  total <- siTotalVolume si
  return $ (siUsedVolume si / total) * 100
instance Semigroup SummaryInfo where
  (SummaryInfo f t u) <> (SummaryInfo f' t' u') =
    SummaryInfo (f `mPlus` f') (t `mPlus` t') (u+u')
instance Monoid SummaryInfo where
  mempty = SummaryInfo Nothing Nothing 0

mPlus :: Maybe Double -> Maybe Double -> Maybe Double
mPlus Nothing x = x
mPlus (Just x) (Just y) = Just (x+y)
mPlus x Nothing =  x

summarizeShelves :: Seq (Shelf s) -> WH (SummaryInfo) s
summarizeShelves shelves | null shelves = return $ SummaryInfo Nothing Nothing 0
summarizeShelves shelves = do
    summaries <- traverse (summaryFromShelf $ const $ return ()) shelves
    let ShelvesSummary{..} = sconcat $ toNonEmpty $ impureNonNull summaries
    let totalVolume = suVolume sShelvesSummary / 1e6
        totalFloor = suSurfaceLW sShelvesSummary / 1e4
        used = suVolume sBoxSummary / 1e6
    return $ SummaryInfo (Just totalFloor) (Just totalVolume) used

summary :: forall s. WH ([[Text]], [Text])  s
summary = do
    ss <- gets shelves >>= mapM findShelf
    -- group shelves using summary property
    -- summary=no mean don't display
    let groups :: Map'.Map (Maybe Text) (Seq (Shelf s))
        groups = Map'.fromList  [ (summaryGroup s, ss')
                                | ss'@(s:<_) <- grouped
                                ]
        grouped = groupAllOn  summaryGroup $ filter (not . flip tagIsPresent "sep") ss
        summaryGroup s = flattenTagValues <$>lookup "summary" (shelfTag s)
    infos' <- traverse summarizeShelves groups
    let infos = Map'.mapWithKey adjust infos'
        adjust tag si = case tag of
          (Just ('_':<_)) -> si {siTotalVolume = Nothing, siFloor = Nothing }
          _ -> si
    let total = mconcat (Map'.elems infos)

        f = tshow . (\x -> x :: Int) . round :: Double -> Text
        renderSI si = let values = [ f . siUsedVolume
                                   , (maybe "" f) .  siTotalVolume
                                   , (maybe "" f) . siFree
                                   , (maybe "" f) . siUsedPercent
                                   , (maybe "" f) . siFloor
                                   ] <*> [si]
                      in values

        _unused_table = Table (Group NoLine (Header . fst <$> Map'.toList infos))
                      (Group SingleLine (map Header (words "used total free %used floor" :: [Text])))
                      (map renderSI $ Map'.elems infos)
                +----+ row (Just "Total") (renderSI total)
    -- return $ (TAscii.render (fromMaybe "<main>") id id table)
    return  ( zipWith (:) ("":  (map (fromMaybe "<main>") $ Map'.keys infos))
                          ( (words "used total free %used floor")
                          : (map renderSI $ Map'.elems infos)
                          )
            , (renderSI total)
            )



_unused_isShelfEmpty s = null `fmap` findBoxByShelf s
-- @todo optimize
_unused_seqFilterM :: (Functor m, Monad m) => (a -> m Bool) -> Seq a -> m (Seq a)
_unused_seqFilterM test s = fmap Seq.fromList (filterM test (toList s))


-- * Shelve report 
-- | Display shelf information including, depth really used
--
shelvesReport :: WH [Text] s
shelvesReport = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  shelvesReportFor ss
  
shelvesReportFor :: [Shelf s] -> WH [Text] s
shelvesReportFor ss = do
  let sorted = sortOn shelfRank ss
      shelfRank shelf = fromMaybe (shelfName shelf) $ getTagValuem shelf "@key"
  ls <- mapM  report_ sorted

  return $ ("name,tags,comment,length,width,height,usedLenght,usedWidth,usedHeight,leftLength,leftWidt,leftHeight,bottom,top" <>
            ",lengthRatio,widthRatio,heightRatio,boxVolume,usedVolume,packingEfficiency"
           ) : ls

  where report_ :: Shelf s -> WH Text s
        report_ shelf = do
          let (Dimension l w h) = minDim shelf
          boxes <- findBoxByShelf shelf
          let Summary{..} = makeBoxesSummary boxes
          let ratio 0 0 = pack $ "100.00%"
              ratio x y = pack $ printf "%0.2f%%" $ x / y * 100
              shelfVolume = l * w * h
              used = ratio suVolume shelfVolume 
              vefficiency = ratio suVolume (suMaxLength * suMaxWidth * suMaxHeight)
              styles = Set.fromList $ map boxStyle boxes
          return $ intercalate "," $
                 [ (shelfName shelf) -- name
                 , intercalate "#" (flattenTags (shelfTag shelf))
                 , intercalate ";" (toList styles)
                 ] <> map tshow
                 [ l,  w,  h
                 , suMaxLength, suMaxWidth, suMaxHeight
                 , (l - suMaxLength), (w - suMaxWidth), (h - suMaxHeight)
                 , (bottomOffset shelf)
                 , (bottomOffset shelf + dHeight (maxDim shelf))
                 ] <>
                 [ (ratio suMaxLength l), (ratio suMaxWidth w), (ratio suMaxHeight h)
                 , tshow suVolume
                 , used
                 , vefficiency
                 ]


-- returns the lis
listShelfTags :: WH ([Text]) s
listShelfTags = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  return . List.nub . sort $ concat (map (flattenTags . shelfTag) ss)

shelfTagsReport :: WH (IO ()) s
shelfTagsReport = do
  tags <- listShelfTags
  return $ (mapM_ putStrLn tags)

-- * box report 
-- | Display list of all individual boxes and their location
boxesReport :: WH (IO ()) s
boxesReport = do
  sbS <- shelfBoxes
  ios <- mapM report_ sbS

  return $ sequence_ ((putStrLn "style,content,location"):ios)

  where -- report_ :: Box s -> WH (IO ()) s
        report_ (shelf, box) = do
          let tilingMode = howManyWithDiagonal (minDim shelf) (maxDim shelf) (boxDim box)
          return $ printf "%s,%s,%s,%s,%s\n" (boxStyle box) (boxContent box) (shelfName shelf) (showOrientationWithDiag (orientation box) tilingMode) (showHowManysFor tilingMode)
  
showHowMany :: HowMany -> Text
showHowMany (HowMany _ l w h) = intercalate "x" $ map tshow [l,w,h]

showHowManys :: NonNull [HowMany] -> Text
showHowManys hms = intercalate "|" $  map showHowMany (toList hms)

showHowManysFor :: TilingMode -> Text
showHowManysFor = showHowManys . tmHowManys

-- * Compatible with warehouse planner input 
-- | Generates moves from actual styles positions, ie find all shelves
-- for  a given styles

-- @TODO make works in the middle or multiple times
groupNames :: [Text] -> [Text]
groupNames = groupNamesLeft

-- | Group names with commont prefix 
-- 
-- >>> groupNamesLeft ["A1", "A2"]
-- ["A[12]"]
-- 
-- But do it only once.
-- >>> groupNamesLeft ["A1x", "A2x", "A1y", "A2y"]
-- ["A1[xy]", "A2[xy]"]
groupNamesLeft :: [Text] -> [Text]
groupNamesLeft names = let
  groups = Map'.fromListWith (<>) [(initEx name, [lastEx name]) | name <- names]

  toName (prefix, "") = prefix
  toName (prefix, x :< "") = prefix <> singleton x
  toName (prefix, xs) = pack $ printf "%s[%s]" prefix (sort xs)

  in map toName (Map'.toList groups)

boxStyleWithTags :: Box s -> Text
boxStyleWithTags b = let
  isVirtual ('\'':<_) = True
  isVirtual _ = False
  tags = filter (not . isVirtual) (getTagList b)
  escapedTags = map (replace "#" "\\#") tags
  in intercalate "#" (boxStyleAndContent b : escapedTags)
 

shelvesToNames :: [Shelf s ] -> [Text]
shelvesToNames = List.nub . sort . map shelfName

generateMoves :: SortBoxes -> Maybe BoxSelector -> (Box s -> Text) -> WH [Text] s
generateMoves sortMode selectorm boxName0 = generateMoves' sortMode (Just "stock_id,location") (Just . boxName0) printGroup selectorm where
     printGroup  boxName' _ shelves = boxName' <> "," <> intercalate "|" (groupNames $ shelvesToNames shelves)
-- generateMoves' :: (Box s -> Text) -> (Box s -> [Text]) -> WH [Text] s
generateMoves' :: (Ord k, Eq k)
               => SortBoxes
               -> Maybe t --  ^ Header
               -> (Box s -> Maybe k) --  ^ box key
               ->  (k -> [Box s] -> [Shelf s]  -> t) --  ^ string from key, boxes and unique shelfnames
               -> Maybe BoxSelector
               -> WH [t] s
generateMoves' sortMode header boxKey0 printGroup selectorm = do
 s'bS <- case selectorm of
          Nothing -> shelfBoxes
          Just sel -> do
                   boxes <- findBoxByNameAndShelfNames sel
                   shelf0 <- defaultShelf
                   mapM (\b -> (,b) <$> findShelf (fromMaybe shelf0 $ boxShelf b )) boxes
 generateMovesFor sortMode header boxKey0 printGroup s'bS 

generateMovesFor :: (Ord key , Monad m) => SortBoxes -> Maybe a -> (Box s -> Maybe key) -> (key -> [Box s] -> [b] -> a) -> [(b, Box s)] -> m [a]
generateMovesFor sortMode header boxKey0 printGroup box'shelfs = do
 let grouped =  case sortMode of
             DontSortBoxes -> [(key, [(box, shelf)]) | (shelf, box) <- box'shelfs, Just key <- [boxKey0 box] ]
             SortBoxes -> let rank (_, box) = (boxKey0 box, boxRank box)
                          in Map'.toList
                           $ Map'.fromListWith (<>) [ (key, [(b, s)])
                                                      | (s,b) <- sortOn (Down . rank) box'shelfs
                                                      , Just key <- [boxKey0 b]
                                                      -- , "mop-exclude" `notElem` boxTags b
                                                      ]
     printGroup' (key, box'shelves) = printGroup key boxes shelves where (boxes, shelves) = unzip box'shelves
 return $ maybe id   (:) header $  map printGroup' grouped
  
-- | Generates files compatible with MOP
-- Boxes tagged with "mop-exception" will use their exact location instead of the group
-- of all shelves for the same style.
-- Boxes with different comment but without exception should display their content and comment
-- but use the location as if they were part of the group.
-- This this achieve by first running the report ignoring the comment (to computes the grouped locations)
-- and doing a lookup to find the final location.
{- rST::mop
Mop export
----------

The location of all the boxes can be exported to MOP (via the
generateMOPLocation). By default, locations are given for each style
(regardless of the content) in the form of the pattern matching all used
shelves. The location of a particular variant (style + content) can
exported separately using the tag ``mop-exception``. Boxes can also be
excluded using the tag ``mop-exclude``. This is particularey usefull to
exclude shelves which shoudn't be taken into account. Example, to
exclude all shelves starting with ``_`` (shelves filtered from the
summary report)

::

   :TAGS:
   stock_id,tag
   /#_*,mop-exclude
   :END:

Arbitrary comments can also be added to a box using the tag
``mop-comment=``\ *``your comment``*. In that case, variant with comment
will be exported separately (like with ``mop-exception``) except the
location will be the location of all the box of the same style (instead
of the exact location of the box itself). To have a global comment (the
same across all variant of the same style) without generating a line per
variant, the tag ``mop-no-exception`` can be used. In case many boxes of
the same group have different comments, the one of the first box will be
used. To force a particula box to be used force, ``@content`` and
``@style`` can be used. Finaly, shelves with the tag ``mop-priority``
will be used first when deciding which shelf is to display first.
Shelves with the tag ``mop-excluded`` will be excluded.
::rST -}
generateMOPLocations :: Maybe BoxSelector -> WH [Text] s
generateMOPLocations selectorm = do
  shelf'boxSMulti <- case selectorm of
                      Nothing -> shelfBoxes
                      Just sel ->  do
                           boxes <- findBoxByNameAndShelfNames sel
                           shelf0 <- defaultShelf
                           mapM (\b -> (,b) <$> findShelf (fromMaybe shelf0 $ boxShelf b )) boxes
  let shelf'boxes = [ (shelf, single)
                  | (shelf, mixed)  <- shelf'boxSMulti
                  , single <- toList $ multipleContentToSingles mixed
                  ]
  locationMap <- Map'.fromList . concat <$> generateMovesFor SortBoxes (Nothing) (boxName False)
                                                  (\_ boxes shelves -> [ (boxStyleAndContent box, shelves)
                                                                       | box <- boxes
                                                                       ])
                                                  shelf'boxes

  generateMovesFor SortBoxes (Just "stock_id,location") (boxName True) (printGroup locationMap) shelf'boxes where
  -- use box style unless the box is tagged as exception
  boxName checkComment box  = let comment = getTagValuem box "mop-comment"
                                  hasTag = tagIsPresent box
                              in case (hasTag "mop-exclude") of
                                 True -> Nothing -- skipped
                                 _ -> let get = if hasTag "mop-exception" 
                                                || ( checkComment
                                                   && not (null comment)
                                                   && not (hasTag "mop-no-exception")
                                                   )
                                                then boxStyleAndContent
                                                else boxStyle
                                      in Just (get box)
 
  -- display shelf we pick from first. This is needed
  -- because the first displayed shelf is used to sort style by location
  sortShelves = List.nub
              . map shelfName
              . sortOn ((,) <$> (Down . flip tagIsPresent  "mop-priority") <*> shelfName)
              . filter (not . flip tagIsPresent "mop-exclude")
  -- truncate 
  groupNames2 name names = case groupNames names of
   (x:y:_:_) -> intercalate "|" [name, x,y] <> " ..."
   xs -> intercalate "|" (name:xs)
  -- add comment from tag
  printGroup locationMap (boxName_) boxes shelves0 = boxName_ <> "," <>
                (encodeCsv $ case (boxComment, sortShelves shelves) of
                       (Nothing, name:names) -> groupNames2 name names 
                       (Just comment, [name]) -> name <> " " <> comment
                       (Just comment, name:names) -> (groupNames2 name names)  <> " " <> comment
                       ( commentM, []) -> fromMaybe "" commentM
                )
    where shelves = Map'.findWithDefault shelves0 boxName_ locationMap 
          boxComment = case boxes of
                          [] -> Nothing
                          box:_ -> getTagValuem box "mop-comment"
          -- put between double quotes and escape tem 
          encodeCsv s = "\"" <> replace "\"" "\"\"" s <> "\""



                                        
-- | Generate a generic report using tags prefixed by the report param
{- rST::generic
A generic report can generated using special tags. All boxes will be
grouped first using the special tag *``report``*\ ``-group`` and then
*``report``*\ ``-key``. For each group (having the same key), a line
will be displayed with the content of *``report``*\ ``-group`` (for the
first level of grouping) and the *``report``*\ ``-value`` property (for
boxes grouped by *``report``*\ ``-key``). The ``report`` prefix can be
changed by setting an alternative prefix in the parameter field. This
allows multiples report to be defined within the same plan. The
following group attributes will be expanded :

.. ihaskell:: Report::expansion

Example, to generate a valid TAG File tagging each box using its unique
barcode tag with its location

::

   :TAGS:
   stock_id,tag
   ,report-key=$[barcode]
   ,"report-value=$[report-key],$location"

Note the presence of quotes wich allow a comma to be used inside the tag
field and the use of ``$[report-key]`` to display the barcode in the
report
::rST -}
generateGenericReport :: Day -> Text -> WH [Text] s
generateGenericReport today prefix = do
  s'bS <- shelfBoxes
  -- group by group if exists
  let groupKey box = getTagValuem box (prefix <> "-group")
      groups0 = Map'.fromListWith (<>) [ (gkey, [s'b])
                                    | s'b <- s'bS
                                    , let gkey = groupKey (snd s'b)
                                    ]
      groups = case Map'.keys groups0 of
        [Nothing] -> groups0
        _ -> Map'.delete Nothing groups0

  rs <- forM (Map'.toList groups) $ \(gkey, s'bs) -> do
    -- display the group only if it's not null
    let withTitle items = case gkey of
                            Just s | not (null s) -> expandReportValue today boxes (shelvesToNames shelves) s : items
                            _ -> items
        (shelves, boxes) = unzip s'bs
    items <- generateGenericReport' today prefix s'bs
    return (withTitle items)
  case concat rs of
    [] -> return ["Empty report for '" <> prefix <> "' . Make sure the tags #" <> prefix
                 <> "-key and #" <> prefix <> "-value are defined"
                 ]
    texts -> return texts
        
  
generateGenericReport':: Day -> Text -> [(Shelf s, Box s)] -> WH [Text] s
generateGenericReport' today prefix s'bs = generateMovesFor SortBoxes Nothing boxKey0 printGroup s'bs where
  boxKey0 box = getTagValuem box (prefix <> "-key")
  printGroup boxKey_ [] __shelfNames = boxKey_
  printGroup boxKey_ boxes@(box:_) shelfNames = let
    value0 = getTagValuem box (prefix <> "-value")
    value = maybe boxKey_ (expandReportValue today boxes $ shelvesToNames shelfNames ) value0
    in value

generateStockTakes :: Maybe BoxSelector ->  WH [Text] s
generateStockTakes selectorm= do
    boxes_ <- case selectorm of
            Nothing -> sortOn boxId <$> findBoxByNameSelector (NameMatches [])
            Just sel -> do
              findBoxByNameAndShelfNames sel
                
    shelf0 <- defaultShelf
    sb <- mapM (\b -> (,b) <$> findShelf (fromMaybe shelf0 $ boxShelf b )) boxes_
    return $ [":STOCKTAKE:"
             ,"Bay No,Position,Style,Length,Width,Height,Orientations"
             ]
             ++ [ printStockTake sb (boxPositionSpec box) 
                | sb@(_, box) <- sb
                ]
             ++ [":END:"]
printStockTake :: (Shelf s, Box s)  -> Text -> Text
printStockTake (shelf, box) posititionSpec = 
     let Dimension l w h = _boxDim box
     in intercalate  ","
        [ shelfName shelf
        , posititionSpec
        , (boxStyleWithTags box)
        , (pack $ printf "%0.2f,%0.2f,%0.f" l w h)
        , (concat $ map showOrientation' $ boxBoxOrientations box)
        ]
-- | Like stocktake but don't use set offest but use a FillCommand syntax
generateFuzzyStockTakes :: Maybe BoxSelector ->  WH [Text] s
generateFuzzyStockTakes selectorm= do
    boxes_ <- case selectorm of
            Nothing -> findBoxByNameSelector (NameMatches [])
            Just sel -> do
              findBoxByNameAndShelfNames sel
    -- boxes need to be sorted by shelf, depth , length and heigt
    -- That is the order they are see
    shelf0 <- defaultShelf
    shelf'boxes_ <- mapM (\b -> (,b) <$> findShelf (fromMaybe shelf0 $ boxShelf b )) boxes_
    let sortedS'B = flip sortOn shelf'boxes_ \(shelf, box) ->
                         let (Dimension ol ow oh, offset) = boxPosition box 
                             hasOffset = offset /= Dimension 0 0 0
                         in (shelfName shelf, hasOffset, ow, ol, oh) 
                
    return $ [":STOCKTAKE:"
             ,"Bay No,Position,Style,Length,Width,Height,Orientations"
             ]
             ++ [  printStockTake sb (posSpecs sb previous)
                | (sb, previous) <-  zip sortedS'B (Nothing : map Just sortedS'B)
                ]
             ++ [":END:"]
    where posSpecs (shelf, box) previous =
                   let (Dimension ol ow oh, offset) = boxPosition box
                       specs = breaks <> orient
                       orient :: [Text]
                       orient = case previous of
                                 Just(pshelf, pbox) | boxDim box == boxDim pbox 
                                                    , pshelf == shelf
                                                    , orientation box == orientation pbox
                                                    -> []
                                 _ ->  [showOrientation' (orientation box) ]
                       breaks :: [Text ]
                       breaks = case previous of
                                Nothing-> []
                                Just (pshelf, pbox) | pshelf == shelf -> 
                                     let Dimension pol pow _poh = boxCoordinate pbox
                                     in if | pow /= ow -> replicate (round $ ow - pow) "nd"
                                                          ++ replicate (round ol - 1) "nc"
                                                          ++ skips
                                           |ol /= pol || oh == 1 -> replicate (round $ ol -pol) "nc" ++ skips
                                           | otherwise -> []
                                _newShelf -> [ "cp" ]
                       skips = replicate (round oh -1) "skip"

                   in case offset of
                        Dimension 0 0 0 -> intercalate " " specs
                        _ -> boxPositionSpec box


-- | Expands group related properties, which can't be processed on a indivial box
-- e.g. boxes counts, shelves list etc ...
expandReportValue :: Day -> [Box s] -> [Text] -> Text -> Text
expandReportValue today boxes shelves s = let
  updates = [ replace "$<count>" (tshow $ length boxes)
            , replace "$<locations>" (intercalate "|" $ groupNames shelves)
            , replace "$<shelves>" (intercalate "|" shelves)
            , replace "$<total-volume>" $ pack $ printf "%0.1f" ((sum $ map boxVolume boxes) * 1E-6)
            , replace "$<style-count>" (tshow $ length styles)
            , replace "$<styles>" $ (intercalate "|" styles)
            , replace "$<content-count>" (tshow $ length contents)
            , replace "$<contents>" $ (intercalate "|" contents)
            , replace "$<sku-count>" (tshow $ length contents)
            , replace "$<skus>" $ (intercalate "|" skus)
            , replace "$<shelf-count>" $ (tshow $ length shelves)
            , replace "$<dimensions-count>" (tshow  $ lengthBy' boxes _boxDim)
            , replace "$<orientations>" orientations
            , replace "$<orientation-count>" (tshow  $ length orientations)
            , replace "$<hash>" "#"
            , replace "$<comma>" ","
            , replace "$<dollar>" "$"
            , replace "$<divide>" "/"
            , replace "$<today>" (pack $ formatTime defaultTimeLocale "%Y-%m-%d" today)
            ]
  orientations = concatMap showOrientation . List.nub . sort $ map orientation boxes
  styles = List.nub . sort $ map boxStyle boxes
  contents = List.nub . sort $ map boxContent boxes
  skus = List.nub. sort $ map boxStyleAndContent boxes
  in foldl' (flip ($)) s updates

{-rST::expansion

-  ``$<count>`` : number of boxes within the group
-  ``$<shelf-count>`` : number of different shelves
-  ``$<locations>`` list of shelves (compacted)
-  ``$<shelves>`` list of shelves
-  ``$<total-volume>`` : total volumes in m\ :sup:`3`
-  ``$<style-count>`` : number of different styles
-  ``$<styles>`` : list of styles separated by a ``|``
-  ``$<content-count>`` : number of different content
-  ``$<contents>`` : list of contents separated by a ``|``
-  ``$<sku-count>`` : number of different SKUs
-  ``$<skus>`` : list of SKUs separated by a ``|``
-  ``$<dimensions-count>`` : number of different dimensions
-  ``$<orientations>`` : different orientations
-  ``$<orientations-count>`` : number of different dimensions
-  ``$<today>`` : today's date with the following format ``YYYY-MM-DD``

Symbols would can't be used without being interpreted by the parser can
be expanded using

-  ``$<hash>`` ``#``
-  ``$<comma>`` ``,``
-  ``$<divide>`` ``/``
-  ``$<dollar>`` ``$``

.. note::
   
   group expansion are between ``$<...>`` to avoid double expansion problem.
::rST
-}

lengthBy' :: (Ord k, Eq k) => [a] -> (a -> k) -> Int
lengthBy' boxes f = length . List.nub . sort $ (map f boxes)

-- replace :: Text -> Text -> Text -> Text
-- replace needle value "" = "
-- replace needle value s@(c:<cs) = case stripPrefix needle s of
--   Nothing -> c `cons` replace needle value cs
--   Just s' -> value <> replace needle value s'

  
  

  
-- * Optimizer 
-- find optimal way to rearrange the warehouse.
-- The first things is to separate what needs to go on the "top" from
-- normal normal shelves
-- next we need to group shelves by similarity

-- then we need get for each style is best shelf and the remaining bits


-- shelfKey :: Shelf s -> (Dimension, Dimension, BoxOrientator, FillingStrategy)
shelfKey = (,,)
         <$> (tweakWidth <$> maxDim)
         <*> shelfBoxOrientator
         <*> shelfFillingStrategy
  where tweakWidth s = s -- { dWidth = 100 }


-- | group identical shelves, keep the first one as example
groupShelves :: (Shelf s -> Bool) -> WH [(Shelf s, [Text])] s
groupShelves exclude = do
  ss <- toList <$> gets shelves >>= mapM findShelf
  let groups = Map'.fromListWith (<>) [ (shelfKey s, [s])
                                      | s <- filter (applyTagSelectors (mapMaybe parseTagSelector ["!sep", "!error"])
                                                                       shelfTag) ss
                                      , exclude s == False
                                      ]
      regroup ss_ = let sorted = sortOn shelfName ss_
                   in (headEx sorted, map shelfName sorted)

  return $ map (regroup) (Map'.elems groups)


-- | Displays shelf groups summary
groupShelvesReport :: WH [Text] s
groupShelvesReport = do
  groups <- groupShelves (const False)
  let sorted = sortOn ( headEx . snd  ) groups
  forM sorted $ \(s, names) -> do
      let d = maxDim s
      return $ tshow (length names) <> " x " <> (headEx names) <> " -- " <> (lastEx names)
        <> " " <> printDim d


-- Displays shelf group Map (ie what belong to  which group)
groupShelvesReport' :: WH (IO ()) s
groupShelvesReport' = do
  groups <- groupShelves (const False)
  return $ do
    forM_ (sortOn (shelfName . fst) groups) $ \(_, names) -> do
      forM_ names $ \name -> putStrLn $ name <> " => " <> (headEx names)

type BoxKey = (Dimension, Text, [Orientation])
boxKeyForGroup :: Box s -> BoxKey
boxKeyForGroup = (,,) <$> _boxDim <*> boxStyle <*> boxBoxOrientations

-- | Group boxes by style and dimension
-- keep the first one as an example
groupBoxes :: (Box s -> Bool) -> WH [(Box s, Int)] s
groupBoxes exclude = do
    boxIds <- toList <$> gets boxes
    boxes <- mapM findBox boxIds
    groupBoxes' (filter (not . exclude) boxes)

groupBoxes' :: [Box s] -> WH [(Box s, Int)] s
groupBoxes' boxes =  do
    let groups = Map'.fromListWith (<>) [ (boxKeyForGroup b, [b])
                                        | b <- boxes
                                        ]
        regroup bs = (lastEx bs, length bs)
    return $ map regroup (Map'.elems groups)

groupBoxesReport :: [Box s] -> WH [Text] s
groupBoxesReport boxes = do
  groups <- groupBoxes' boxes
  let withVolumes = [(volume (boxDim b) * (fromIntegral q), b'q) | b'q@(b,q) <- groups ]
  let sorted = map snd $ sortOn (\(rank, (volume_, _)) -> (rank, Down volume_)) (zip [1..] withVolumes)
  forM sorted $ \(v, (box, qty)) -> do
      let d = _boxDim box
      return $ (boxStyle box ) <> (concatMap showOrientation (boxBoxOrientations box )) <>   " x "  <> tshow qty
               <> " " <> f (dLength d) <> "," <> f(dWidth d) <> "," <> f (dHeight d)
               <> " " <> f (v/1000000 ) <> "m^3"
      where f = pack . printf ("%0.1f")

-- | Temporary data to stock "residuals"
-- i.e. how  many boxes can fit in the given shelw
-- and how many are left
data Residual s = Residual
  { rBox :: Box s
  , rShelf :: Shelf s
  , rNumberOfShelves :: Int -- ^ number of available shelves
  , rBoxPerShelf :: !Int -- ^
  , rUsedShelves :: !Int -- ^ number of shelves fully used
  , rBoxLeft :: !Int
  , rOrientation :: !Orientation
  , rVolumeLeftForFull :: !Double -- ^ Volume left for a full shelf
  , rVolumeUsedForLO :: !Double -- ^ Volume used for a left over shelf
  , rVolumeLeftForLO :: !Double -- ^ Volume left for left over shelf
  , rPercentageUsedForFull :: !Int -- ^ Percentage of volume used for a full shelf. Only if full shelf needed
  } deriving (Eq, Show)

rNumberOfBox r = rUsedShelves r * rBoxPerShelf r + rBoxLeft r
rVolumeLeftForAllFull r = rVolumeLeftForFull r * (fromIntegral $ rUsedShelves r)
-- | Define Ord for residuals so that "best" residuals are first
-- best minimize the wasted space

findResidual :: [OrientationStrategy] -> Shelf s -> Box s -> Int -> Int -> Maybe (Residual s)
findResidual orientations shelf box qty nbOfShelf = let
  sdim = maxDim shelf
  bdim = _boxDim box
  (or,tilingMode, ()) = bestArrangement orientations [(minDim shelf, sdim, ())] bdim
  in case tmTotal tilingMode of
    0 -> Nothing
    nbPerShelf -> let
      (used, left) = qty `divMod` nbPerShelf
      volLeftForFull = if used == 0
                  then 0
                  else volume sdim - (fromIntegral nbPerShelf) * (volume bdim)
      volUsedLO = (fromIntegral left) * (volume bdim)
      volLeftOver = if left  == 0
                      then 0
                      else volume sdim - volUsedLO
      perc = if volLeftForFull == 0 then 100 else round $ (1- volLeftForFull / volume sdim) * 100
      in Just $ Residual box shelf nbOfShelf nbPerShelf used left or (volLeftForFull / 1000000) (volUsedLO / 1000000) (volLeftOver / 1000000) perc
                         

-- | Try each each pair of boxes and each shelves to find the best arrange
-- a pair of style is good if it minimize the occupied ration

data MixedResult = LeftOnly Int  | RightOnly Int | Both Int Int deriving (Eq, Show, Ord)
data Pair s  = Pair
  { pRes1 :: Residual s
  , pRes2 :: Maybe (Residual s)
  , pWastedVolume :: Double
  , pN1 :: Int -- ^ number of full shelves for box 1
  , pN2 :: Int -- ^ number of full shelves for box 2
  , pMixed :: MixedResult
  , pMixedLeft :: Double -- ^ total volume left in the mixed area if any
  } deriving (Eq, Show)

newPair :: Warehouse s -> Residual s -> Maybe (Residual s) -> Maybe (Pair s)
newPair _ res Nothing  =  let
  n1 = rUsedShelves res
  wastedVolume = rVolumeLeftForLO res + rVolumeLeftForAllFull res
  in Just $ Pair res Nothing wastedVolume n1 0 (LeftOnly (rBoxLeft res)) (rVolumeLeftForLO res)

newPair wh res (Just res') =
  if rVolumeLeftForLO res < rVolumeUsedForLO res'
  then Nothing
  else
    -- check if both residuals can actually fit
    let addShelf shelf = do
          newShelf (shelfName shelf)
            (Just $ intercalate "#" $ flattenTags $  shelfTag shelf)
            (maxDim shelf)
            (maxDim shelf)
            0
            (shelfBoxOrientator shelf)
            (shelfFillingStrategy shelf)
        addBoxes res_ shelf = do
          let box = rBox res_
          forM [1..rBoxLeft res_] $ \_ -> newBox (boxStyle box)
                                             (boxContent box)
                                             (_boxDim box)
                                             (rOrientation res_) -- current orientation
                                             shelf
                                             (boxBoxOrientations box)
                                             (getTagList box)

        tryW = do
          -- traceShowM ("NEW PAIR", res, res')
          def <- addShelf (rShelf res)
          shelf <- addShelf (rShelf res)
          boxes <- addBoxes res def
          boxes' <- addBoxes res' def
          inEx <- moveBoxes PRightOnly SortBoxes boxes [(ExitOnTop, [shelf])]
          inEx' <- moveBoxes PRightOnly SortBoxes boxes' [(ExitOnTop, [shelf])]
          -- shelfR <- findShelf shelf

          -- traceShowM ("RES", length boxes, length left, length boxes', length left')
          return (length boxes, length $ excludedList inEx, length boxes', length $ excludedList inEx')


        ors = boxOrientations wh
        wh' = (emptyWarehouse (whDay wh)) {boxOrientations=ors}
        (toFit, notFit, toFit', notFit' ) = unsafePerformIO $ unsafeSTToIO (evalStateT tryW wh')

      in case (notFit, notFit') of
          (0,0) -> -- the two partial shelves we need to extract the contribution
                  -- of the left boxes. For that we calculate the volume left in
                  -- in the mixed shelf and share it using the pro-rata of the overall volume used
                  let used = rVolumeUsedForLO res
                      used' = rVolumeUsedForLO res'
                      totalWasted = (shelfVolume (rShelf res) / 1000000 - used - used')
                      wastedVolume = totalWasted*used/(used+used')
                  in Just $ Pair res (Just res') (rVolumeLeftForAllFull res + wastedVolume)
                        (rUsedShelves res) (rUsedShelves res') (Both toFit toFit')
                        (totalWasted)
          -- does not mix
          (0, _) -> Nothing
          (_, _) -> error "Bug"

pTotalShelves :: Pair s -> Int
pTotalShelves = go <$> pN1 <*> pN2 <*> pMixed where
  go p1 p2 mix = p1 + p2 + case mix of
    LeftOnly 0 -> 0
    RightOnly 0 -> 0
    _  -> 1




findBestPairings :: Warehouse s ->  Map'.Map BoxKey [Residual s] -> [Box s] -> Box s -> [Pair s]
findBestPairings _ _ [] _ = []
findBestPairings wh residualMap boxes box =
  let residuals = lookupResidual residualMap box
      monopairs = catMaybes $ [ newPair wh r Nothing | r <- residuals]

  -- create a temporary map shelf -> to residual for current box
      resMap = Map'.fromList [(shelfKey (rShelf r), r) | r <- residuals ]
      pairs = do
          box' <- boxes -- each other box

          res' <- lookupResidual residualMap box' -- each shelf via residuals
          let shelf = rShelf res'
          res <- maybeToList $ Map'.lookup (shelfKey shelf) resMap
          maybeToList $ newPair wh res (Just res')

  in sortOn pWastedVolume (pairs <> monopairs)


lookupResidual :: Map'.Map BoxKey [Residual s] -> Box s -> [Residual s]
lookupResidual residualMap box = fromMaybe [] $ Map'.lookup (boxKeyForGroup box) residualMap



-- | List all pairs of style and sorted efficiency
-- Residual (shelf,box) with insufficient ration of volume occupation
-- can be filtered.
reportPairs :: Int-> WH (IO ()) s
reportPairs percThreshold = do
  -- removes every boxes which are already in and the shelf itself
  ss <- mapM findShelf =<< toList <$> gets shelves

  shelfBoxes_ <- catMaybes <$> forM (filter (applyTagSelectors (mapMaybe parseTagSelector ["!sep", "!error"]) shelfTag) ss) ( \shelf -> do
    boxes <- findBoxByShelf shelf
    return $ if null boxes then Nothing else Just (shelfName shelf, map boxKey boxes)
    )


  let shelvesToSkip = Set.fromList (map fst shelfBoxes_ )
      boxesToSkip = Set.fromList (concatMap snd shelfBoxes_)
      isTop = maybeToList (parseTagSelector "top")
    
  boxGroups <- groupBoxes (\b -> boxKey b `Set.member` boxesToSkip)
  shelfGroups <- groupShelves (\s -> shelfName s `Set.member` shelvesToSkip || applyTagSelectors isTop shelfTag s)

  boxo <- gets boxOrientations

  wh <- get

  let residualMap = Map'.fromList
        [ (boxKeyForGroup box, residuals)
        | (box, qty) <- boxGroups
        , let residuals = [ residual
                          | (shelf, shelves) <- shelfGroups
                          , residual <- maybeToList $ findResidual (boxo box shelf) shelf box qty (length shelves)
                          , rPercentageUsedForFull residual > percThreshold
                          ]
        ]

  let boxGroupWeight (box, qty) = Down $ (fromIntegral qty) * boxVolume box
  let sortedBoxGroups = sortOn (boxGroupWeight) boxGroups
  let boxes = map fst sortedBoxGroups

  let pairsS = map (findBestPairings wh residualMap boxes) boxes

  return $ mapM_ (mapM_  (putStrLn . showPair) {-. take 500-})  pairsS

showPair pair = let res = pRes1 pair
                    shelf = rShelf res
                    nbOfShelves = rNumberOfShelves res
  in case (pRes2 pair, pMixed pair) of
    (Just res', Both q q') ->  (boxStyle . rBox $ res) <>
                  (pack $ printf "x%02d" (rNumberOfBox res)) <> (showOrientation $ rOrientation res) <> " <=> "
                  <> (pack $ printf "%02dx" $ rNumberOfBox res')
                  <> (boxStyle $ rBox res' ) <> (showOrientation $ rOrientation res')
                  <> " [[ " <> (shelfNameTag shelf )
                  <> (pack $ printf" (%d|%d) ]] " nbOfShelves (nbOfShelves - pTotalShelves pair))
                  <> tshow (pN1 pair) <> ":"<> (tshow $ rPercentageUsedForFull res) <> "% | " <> tshow q <> "<>" <> tshow q' <> " | "
                  <> tshow (pN2 pair) <> ":" <> (tshow $ rPercentageUsedForFull res') <> (pack $ printf "mixLeft;%0.1f " (pMixedLeft pair)) <> (pack $ printf "%% wasted:%0.2f  " (pWastedVolume pair))
                  <> printDim (boxDim $ rBox res) <> (pack $ printf "(%d)" $ rBoxPerShelf res) <> " || " <> printDim (boxDim $ rBox res') <> (pack $ printf "(%d)" $ rBoxPerShelf res')

    (_, LeftOnly q ) -> (boxStyle . rBox $ res)
                  <> (pack $ printf "x%02d" (rNumberOfBox res)) <>" <=   0 "
                  <> " [[ " <> (shelfNameTag shelf) <> (pack $ printf " (%d|%d) ]] "
                                                                   nbOfShelves
                                                                   (nbOfShelves - pTotalShelves pair))
                  <> tshow (pN1 pair) <> ":" <> (tshow $ rPercentageUsedForFull res) <> "%  | " <> tshow q <> " _ | "
                  <> (pack $ printf " wasted:%0.2f  " (pWastedVolume pair))
                  <> printDim (boxDim $ rBox res) <> (pack $ printf "(%d)" $ rBoxPerShelf res)
    _ -> error "Bug"


--

generateBoxesHistory :: Maybe BoxSelector -> WH [Text] s
generateBoxesHistory selectorm = do
    events <- gets whEventHistory
    
    boxes_ <- case selectorm of
            Nothing -> sortOn boxId <$> findBoxByNameSelector (NameMatches [])
            Just sel -> do
              findBoxByNameAndShelfNames sel
    boxhistorys <- mapM getBoxHistory boxes_
    boxReports <- mapM boxHistory boxhistorys
    return $ map displayEvent events
           <> concatMap history boxReports
    where history xs = replicate 50 '*' : xs
           
boxHistory :: History Box s -> WH [Text] s
boxHistory e'boxs@((_,lastBox) :| _) = do
    history <- mapM go e'boxs
    return $  tshow lastBox
                      : boxStyleWithTags lastBox <> " " <> tshow (boxShelf lastBox) <> " " <> boxPositionSpec lastBox
                      : concatMap toList history
    where go (ev,box) = do
             shelfm <- forM (boxShelf box) findShelf
             return [ tshow ev
                    <> "\t" <> evDescription ev
                    <> "\t" <> boxStyleWithTags box
                    <> "\t" <> maybe "∅" shelfName shelfm
                    <> "\t" <> boxPositionSpec box
                    ]
       
     

generateTags :: Maybe BoxSelector -> WH [Text] s
generateTags selectorm = do
  boxes <- case selectorm of
             Nothing -> sortOn boxId <$> findBoxByNameSelector (NameMatches [])
             Just sel -> do
                findBoxByNameAndShelfNames sel
  return $ "tag,value,sku,box_id"
         : [ intercalate "," [ tag, value, boxStyleAndContent box, tshow (boxId box)]
           | box <- boxes
           , (tag,values) <- Map.toList $ boxTags box
           , value <- toList values
           ]



  

