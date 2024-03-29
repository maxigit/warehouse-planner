{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
module WarehousePlanner.Base
( applyNameSelector
, applyTagSelectors
, aroundArrangement 
, assignShelf
, bestArrangement
, bestPositions, bestPositions'
, boxCoordinate
, boxContentPriority
, boxPosition
, boxPositionSpec
, boxRank
, boxStyleAndContent
, buildWarehouse
, clearCache
, cornerHull
, defaultShelf
, deleteBoxes
, deleteShelf
, emptyWarehouse
, expandAttribute
, expandAttributeMaybe
, extractTag
, extractTags
, extractModes
, filterBoxByTag
, filterShelfByTag
, findBoxByNameAndShelfNames
, findBoxByNameSelector
, findBoxByShelf
, findShelfBySelector
, findShelfBySelectors
, findShelvesByBoxNameAndNames
, howMany, howManyWithDiagonal
, incomingShelf
, indexToOffsetDiag, d0, r
, matchName
, maxUsedOffset
, modifyTags
, module WarehousePlanner.Type
, moveBoxes, SortBoxes(..)
, negateTagOperations
, newBox
, newShelf
, orTrue
, parseBoxSelector
, parseShelfSelector
, parseSelector
, parseNameSelector
, parseTagOperation
, parseTagOperations
, parseTagSelector
, parsePositionSpec
, parseOrientationRule
, parseMatchPattern
, printDim
, readOrientations
, replaceSlashes
, shelfBoxes
, stairsFromCorners
, TagOperationF(..)
, Tag'Operation
, updateBox
, updateBoxTags
, updateBoxTags'
, updateShelf
, updateShelfTags
, usedDepth
, Slices
, unconsSlices
, dropTillSlice, dropTillSlot
, withBoxOrientations
)
where
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import qualified Prelude
import Text.Printf(printf)
import Data.Map.Strict qualified as Map'
import Data.Map.Lazy qualified as Map
import Control.Monad.State(gets, get, put, modify)
import Data.Map.Merge.Lazy(merge, preserveMissing, mapMaybeMissing, zipWithMaybeMatched)
-- import Data.List(sort, sortBy, groupBy, nub, (\\), union, maximumBy, delete, stripPrefix, partition)
import Data.List(cycle)
import Data.List.NonEmpty(unzip)
import Data.List qualified as List
import Data.STRef
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import WarehousePlanner.Type
import WarehousePlanner.Slices
import WarehousePlanner.SimilarBy
import Diagrams.Prelude(white, black, darkorange, royalblue, steelblue)
import Data.Text (splitOn, uncons, stripPrefix)
import Data.Text qualified as T 
import Data.Char (isLetter, isDigit)
import Data.Time (diffDays)
import Data.Semigroup (Arg(..))

import System.FilePath.Glob qualified as Glob
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P
import Control.Monad.Fail 
import GHC.Prim 
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import WarehousePlanner.Affine


-- import Debug.Trace qualified as T

sortOnIf f xs0 = 
  if isSorted snd xs
  then xs0
  else fmap fst $ sortOn snd xs
  where xs = fmap (\x -> (x, f x)) xs0
  
  
isSorted f (x:xs@(y:_)) = if f x  /= f y
                        then False
                        else isSorted f xs
isSorted _ _ = True

-- | Internal types to deal with tag and tag operations
-- we use a parametrized type only to get fmap for free
data TagOperationF s = -- ClearTagValues  use SetValue []
                    SetTag -- no value
                  | RemoveTag 
                  | SetValues [s]
                  | AddValue s
                  | RemoveValue s
                  deriving (Eq, Show, Functor, Foldable, Traversable)

type TagOperation = TagOperationF Text
type Tag'Operation = (Text, TagOperation)

-- | Computes the max length or height used within a shelf
-- | by all the boxes. Should be able to be cached.
-- maxUsedOffset :: (Dimension -> Double) -> Shelf -> WH
maxUsedOffset ::  Shelf s -> WH Dimension s
maxUsedOffset shelf = do
    boxes <- findBoxByShelf shelf
    return $ maxDimension $ map boxCorner boxes

-- | Nested groups of shelves, used for display

buildWarehouse :: NonEmpty (NonEmpty (NonEmpty Text)) -> WH (RunsWithId s) s
buildWarehouse xsss = mapM buildRun xsss

buildRun :: NonEmpty (NonEmpty Text) -> WH (Run NonEmpty (ShelfId s) ) s
buildRun xss = mapM buildBay xss

buildBay :: NonEmpty Text -> WH (Bay NonEmpty (ShelfId s) ) s
buildBay xs = do
    idsS <- mapM ( \x ->  do
            case x of
                (uncons -> Just ('-', name)) ->  fmap (map shelfId) $ updateShelfByName (\s -> s { flow = RightToLeft }) name
                name ->  findShelfBySelector (parseSelector name)
            ) xs
    shelves <- mapM findShelf (concat idsS)
    let sorted = sortOn shelfName shelves

    case nonEmpty $ map shelfId sorted of
      Nothing -> error $ "Bay '" ++ (unpack $ unwords $ toList xs ) ++ "' is empty"
      Just bay -> return bay

 -- -| shelf use as error, ie everything not fitting anywhere
defaultShelf :: WH (ShelfId s) s
defaultShelf = (flip Seq.index) 0 <$> gets shelves

-- | shelf use to put incoming boxes
incomingShelf :: WH (ShelfId s) s
incomingShelf = do
  shelves <- gets shelves
  return $ Seq.index shelves (min 1 (length shelves -1))

findBoxByShelf :: Shelf' shelf => shelf s -> WH [Box s] s
findBoxByShelf shelf = do
  boxIds <- _shelfBoxes <$> findShelf shelf
  mapM findBox $ toList boxIds


findBoxByNameSelector :: (NameSelector (Box s)) -> WH [Box s] s
findBoxByNameSelector selector = do
  boxIdMap <- gets boxMap
  let matcher = applyNameSelector (coerce selector) id
  boxess <- forM (Map.toList boxIdMap)
                \(style, ids) ->
                  if matcher style
                  then
                    mapM findBox ids
                  else return mempty
  return $ toList $  mconcat boxess

findShelfByBox :: Box' box => box s -> WH (Maybe (ShelfId s)) s
findShelfByBox box' = do
  box <- findBox box'
  return $ boxShelf box

-- | find shelf by name and tag
findShelfBySelector :: Selector (Shelf s) -> WH [ShelfId s] s
findShelfBySelector selector = map shelfId <$> findShelfBySelector' selector
findShelfBySelector' :: Selector (Shelf s) -> WH [Shelf s] s 
findShelfBySelector' (Selector nameSel tagSels ) = do
  shelfIds <- toList <$> gets shelves
  shelves0 <-  filterByNameSelector (mapM findShelf shelfIds) shelfName  nameSel
  let shelves1 = filter (applyTagSelectors tagSels shelfTag) shelves0
  return shelves1
-- | Find shelf by name and tag
-- but respect alternative order
-- ie B|A will return B before A
-- B*|A* will return Bs (sorted alphabetically) the A* (alphabetically)
findShelfBySelectors :: [Selector (Shelf s)] -> WH [ShelfId s] s
findShelfBySelectors selectors = do
  shelvess <- mapM findShelfBySelector' selectors
  return [ shelfId shelf
         | shelves <- shelvess
         , shelf <- sortOn shelfName shelves
         ]


matchName :: Text -> NameSelector s
matchName name = NameMatches [MatchFull name]

filterShelfByTag :: [TagSelector (Shelf s)] -> Shelf s -> Bool
filterShelfByTag selectors shelf = applyTagSelectors selectors shelfTag shelf

filterBoxByTag :: [TagSelector (Box s)]-> Box s -> Bool
filterBoxByTag selectors box =  applyTagSelectors selectors boxTags box

-- | Compiles a match against a glob pattern if Necessary
specials = "*?[]{}<>" :: String
isGlob :: Text -> Bool
isGlob s = case break (`List.elem` specials) s of
  (_, uncons -> Just _) -> True
  _ -> False

filterByNameSelector :: WH [a s] s -> (a s -> Text) -> (NameSelector (a s)) -> WH [a s] s
filterByNameSelector objects objectName selector = do
   let matcher= applyNameSelector selector objectName
   filter matcher <$> objects

parseBoxNumberSelector :: Text -> BoxNumberSelector
parseBoxNumberSelector "" = BoxNumberSelector Nothing Nothing Nothing
parseBoxNumberSelector s = case P.parse parser (unpack s) s of 
  Left err -> error (show err)
  Right expr -> expr
  where parser = do
          limits <- P.optionMaybe parseLimit `P.sepBy` P.char '^'
          case limits of 
               (_:_:_:_:_) -> fail "Too many limits in"
               _ -> let (content: shelves: total:_) =  limits ++ cycle [Nothing]
                    in return $ BoxNumberSelector content shelves total
                
-- | Parsel [[tag]|{attribue}][min:][max]
parseLimit :: P.Parser Limit      
parseLimit = do
  reverse <- P.option False (P.char '-' >> return True)
  keys <- P.many (parseTag <|> parseAttribute)
  minM <- P.optionMaybe $ P.many1 P.digit
  maxMM <- P.optionMaybe $ P.char ':' >> P.optionMaybe (P.many1 P.digit)
  let (start, end) = case (minM >>= readMay,  fmap (>>= readMay) maxMM ) of
        -- :max or :
        -- (Nothing, Just maxm) -> (Nothing, maxm)
        -- max
        (Just min_, Nothing) -> (Nothing, Just min_)
        -- min:
        (Just min_, Just Nothing) -> (Just min_, Nothing)
        (minm, Just maxm) -> (minm, maxm)
        (Nothing, Nothing) -> (Nothing, Nothing)
  return $  Limit start end keys reverse
  where parseTag = OrdTag . pack <$> do P.char '[' >> P.many1 (P.noneOf "]") <* P.char ']'
        parseAttribute = OrdAttribute . pack <$> do P.char '{' >> P.many1 (P.noneOf "}") <* P.char '}'


-- | TODO Should be true be seems to work like that
-- this will mean, that we need a normal or 
orTrue :: [Bool] -> Bool
orTrue [] = False
orTrue bs = or bs
-- | Find box for a given style but only belonging
-- to the given shelves. This allows to only move
-- boxes in error or coming and leave the current box
-- of a particular style to stay where they are.
-- We can also specify a maximum number to pick
-- it needs to be before the shelf condition
--
-- syntax is  Box#tag^3/shelf#tag : 3 box from shelf shelf
findBoxByNameAndShelfNames :: BoxSelector s -> WH [Box s] s
findBoxByNameAndShelfNames ( BoxSelector ( Selector (NameMatches [])
                                                    [ TagIsKeyAndValues (MatchFull prop)
                                                                        [MatchFull value]
                                                    ]
                                         ) shelfSel numSel
                           ) = do
  -- optimisation to find box by prop=value
  -- we create a Map and cache it in case it is reused later
  boxMap <- getOrCreateBoxTagMap prop 
  -- TODO factorize with below
  let allBoxes  = fromMaybe [] (lookup value boxMap)
  box'shelfms <- forM allBoxes $ \box -> do
      shelfIdM <- findShelfByBox (boxId box)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if applyNameSelector (nameSelector shelfSel) shelfName shelf'
                                      && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'shelfS =  [ (box, shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map fst $ limitByNumber numSel box'shelfS


findBoxByNameAndShelfNames (BoxSelector boxSel shelfSel numSel) = do
  -- all boxes matching name
  allBoxesBeforeTag <- findBoxByNameSelector (nameSelector boxSel)
  let allBoxes = filter (applyTagSelectors (tagSelectors boxSel) boxTags) allBoxesBeforeTag
  box'shelfms <- forM allBoxes $ \box -> do
      shelfIdM <- findShelfByBox (boxId box)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if applyNameSelector (nameSelector shelfSel) shelfName shelf'
                                      && applyTagSelectors (tagSelectors shelfSel) shelfTag shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'nameS =  [ (box, shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map fst $ limitByNumber numSel box'nameS


-- | Limit a box selections by numbers
limitByNumber :: BoxNumberSelector -> [(Box s, Shelf s)] -> [(Box s, Shelf s)]
limitByNumber selector boxes0 = let
  sorted = sortOnIf (boxFinalPriority selector) boxes0
  sndOrSel (box, shelf) = keyFromLimitM (nsPerShelf selector) (Right $ shelfName shelf) box shelf
  boxes1 = maybe id (limitBy (pure . pure . boxSku . fst)) (nsPerContent selector) $ sorted
  boxes2 = maybe id (limitBy sndOrSel) (nsPerShelf selector) $ boxes1
  boxes3 = maybe id take_ (nsTotal selector) $ sortOnIf (boxFinalPriority selector) boxes2
  --                            -- ^ things might have been shuffle by previous sorting , so resort them                                                         
  -- limitBy :: Ord  k => ((Box s, Text) -> k) -> Limit -> [(Box s, Text)] -> [(Box s, Text)]
  limitBy key n boxes = let
    sorted = sortOnIf (boxFinalPriority selector) boxes
    group_ = Map'.fromListWith (flip(<>)) [(key box, [box]) | box <- sorted]
    limited = fmap (take_ n . sortOnIf (snd . boxFinalPriority selector) ) group_
    in concat (Map'.elems limited)
  take_ :: Limit -> [a] -> [a]
  take_ sel = maybe id (drop . (subtract 1)) (liStart sel) . maybe id take (liEnd sel) . rev
    where rev = if liReverse sel then reverse else id
  in boxes3

keyFromLimitM :: Maybe Limit -> Either Int Text -> Box s -> Shelf s ->  [Either Int Text]
keyFromLimitM limit def box shelf =
  case liOrderingKey =<< toList limit of
    [] -> [def]
    keys -> map evalKey keys
  where evalKey k = case k of
          OrdTag tag0 -> let (tag, evaluator) = parseEvaluator tag0
                         in case evaluator $ getTagValuesWithPresence box tag of
                                 Nothing -> Right maxString
                                 Just v -> maybe (Right v) Left (readMay v) -- :: Either Int Text
          OrdAttribute att -> expandIntrinsic att box shelf
        maxString = T.replicate 100 (singleton maxBound)


-- limitBy :: Ord k => (Box s -> k) -> Int -> [Box s] -> [a]
  
boxFinalPriority :: BoxNumberSelector -> (Box s, Shelf s) -> ([Either Int Text] , (Text, [Either Int Text], Text , [Either Int Text]))
boxFinalPriority BoxNumberSelector{..} (box, shelf) = let -- reader
  with selm p = keyFromLimitM selm (Left $ p box) box shelf
  global = with nsTotal boxGlobalPriority
  style = with nsPerShelf boxStylePriority
  content = with nsPerContent boxContentPriority
  in (global, (boxStyle box, style, boxContent box, content))

  
-- | Use similar syntax to boxes but returns shelves instead
findShelvesByBoxNameAndNames :: ShelfSelector s -> WH [Shelf s] s
findShelvesByBoxNameAndNames (ShelfSelector SelectAnything shelfSel) = findShelfBySelector shelfSel >>= mapM findShelf
findShelvesByBoxNameAndNames (ShelfSelector (Selector boxNameSel boxTagSel) shelfSel) = do
  shelves0 <- findShelfBySelector shelfSel >>= mapM findShelf
  -- only keep shelf for which boxes are ok
  let keepShelf shelf = do
        boxes0 <- findBoxByShelf shelf
        boxes <- filterByNameSelector (return boxes0) boxStyle boxNameSel
        case boxes of
          [] -> return False -- box selector is not null we need some boxes
          _ -> do
            return $ all (filterBoxByTag boxTagSel) boxes
  filterM keepShelf shelves0



  


defaultBoxOrientations box shelf =
    let orientations = case (shelfBoxOrientator shelf)  of
            (DefaultOrientation) ->  [up, rotatedUp]
            (ForceOrientations orientations_) -> orientations_
            (BoxOrientations) -> case boxBoxOrientations box of
                                    [] ->  [up, rotatedUp]
                                    ors -> ors
            FilterOrientations orientations_ -> boxBoxOrientations box List.\\ orientations_
            AddOrientations lefts_ rights_ -> lefts_ `List.union` boxBoxOrientations box `List.union` rights_
    in map (\o -> OrientationStrategy o 0 1 Nothing Nothing True) orientations

defaultBoxStyling = BoxStyling{..} where
  foreground = black
  background = white
  circleBgs = []
  border = Nothing
  title = []
  barTitle = Nothing
  displayBarGauge = True
  offsetBarGaugeX = Nothing
  offsetBarGaugeY = Nothing

defaultShelfStyling = ShelfStyling{..} where
  foreground = black
  background = white
  maxBackground = steelblue
  barForeground = black
  barBackground = darkorange
  border = royalblue
  title = []
  barTitle = Nothing
  displayBarGauge = True
emptyWarehouse :: Day -> Warehouse s
emptyWarehouse today = Warehouse mempty mempty (error "No shelves defined")
                                 (const defaultBoxStyling)
                                 (const defaultShelfStyling)
                                 defaultBoxOrientations Nothing today
                                 0

newShelf :: Text -> Maybe Text -> Dimension -> Dimension -> Double -> BoxOrientator -> FillingStrategy -> WH (Shelf s) s
newShelf name tagm minD maxD bottom boxOrientator fillStrat = do
        let -- tags = case splitOn "#" <$> tagm of
            --   Nothing -> mempty
            --   Just [""] -> mempty
            --   Just tags' -> fromMaybe mempty $ modifyTags (map parseTagOperation tags') mempty
            tags = fromMaybe mempty $ fmap parseTagOperations tagm >>= (flip modifyTags mempty)
        uniqueRef@(Arg _i ref) <- newUniqueSTRef (error "should never been called. Base.hs:327")
        let shelf = Shelf (ShelfId_ uniqueRef) mempty name tags minD maxD LeftToRight boxOrientator fillStrat bottom
        lift $ writeSTRef ref shelf

        modify \warehouse ->  warehouse { shelves = shelves warehouse |> ShelfId_ uniqueRef }
        updateShelfTags [] shelf

newBox :: Shelf' shelf => Text -> Text ->  Dimension -> Orientation -> shelf s  -> [Orientation]-> [Text] -> WH (Box s) s
newBox style content dim or_ shelf ors tagTexts = do
    let tags' = map (parseTagOperation . omap replaceSlash) tagTexts
        dtags = dimensionTagOps dim
        -- create "'content" tag
        contentTag = (omap replaceSlash $ cons '\'' content, SetTag)
        tags = fromMaybe mempty $ modifyTags (contentTag : makeContentTags content <> tags' <> dtags) mempty
                                  --   ^ apply dimension tags after tags so dimension override tags

    uniqueRef@(Arg _ ref) <- newUniqueSTRef (error "should never been called. undefined. Base.hs:338")
    let box = Box (BoxId_ uniqueRef) (Just $ shelfId shelf) style content dim mempty or_ ors tags defaultPriorities (extractBoxBreak tags)
    shelf' <- findShelf shelf
    linkBox (BoxId_ uniqueRef) shelf'
    lift $ writeSTRef ref box
    -- modify \warehouse ->  warehouse { boxes = boxes warehouse |> BoxId_ uniqueRef }
    modify \warehouse ->  warehouse { boxMap = snd $ Map.insertLookupWithKey (\_ new old -> old <> new)
                                                                      style (Seq.singleton $ BoxId_ uniqueRef)
                                                                      (boxMap warehouse)
                                    }
    return box
-- |  create #content1=A, #content2=B from A&B
makeContentTags :: Text -> [Tag'Operation]
makeContentTags content = 
  case splitOn "&" content of
   [_] -> []
   contents -> ("mixed", SetTag) : [ ("content" <> tshow i, SetValues [c])
                                   | (c, i) <- zip contents [1..] 
                                   ]

newUniqueSTRef :: a s -> WH (Arg Int (STRef s (a s))) s
newUniqueSTRef object = do
  ref <- lift $ newSTRef object
  unique0 <- gets whUnique
  let unique = unique0 + 1
  modify (\w -> w { whUnique = unique })
  return $ Arg unique ref

-- | Parses a string  to Tag operation
-- the general syntax [-]tag[=[-+]value]
-- tag create the tag if n
-- -tag remove the tag
-- tag=val set the value
-- tag=+val or tag+=val add the value to the existing value
-- tag=-val or tag-=val and -tag=val remove the value
parseTagOperation :: Text -> Tag'Operation
parseTagOperation s = 
  case break (== '=') s of
    (uncons -> Just ('-', tag), "")           -> (tag, RemoveTag)
    (tag, "")               -> (tag, SetTag )
    (uncons -> Just ('-', tag), "=")      -> (tag, SetValues [] ) -- clear tag
    (uncons -> Just ('-', tag), (uncons -> Just ('=', val)))      -> (tag, RemoveValue val )
    (uncons -> Just ('-', tag), val)          -> (tag <> val, RemoveTag)
    (tag, (stripPrefix "=-" -> Just val))      -> (tag, RemoveValue val)
    -- (tag, (stripPrefix "-=" -> Just val))      -> (tag, RemoveValue val)
    (tag, (stripPrefix "=+" -> Just val))      -> (tag, AddValue val)
    -- (tag, (stripPrefix "+=" -> Just val))      -> (tag, AddValue val)
    (tag, (uncons -> Just ('=', val)))          -> (tag, SetValues $ split val)
    (tag, val)              -> (tag <> val , SetTag)
  where split = splitOn ";"
 
parseTagOperations :: Text -> [Tag'Operation]
parseTagOperations tag =
 case splitOn "#" tag of
   [] -> []
   [""] -> []
   tags -> map parseTagOperation tags
  
  
  
negateTagOperations :: [Tag'Operation] -> [Tag'Operation]
negateTagOperations tags = do
  (tag, op) <- tags
  map (tag, ) $ case op of
    RemoveTag -> [SetTag]
    SetTag -> [RemoveTag]
    SetValues values -> map RemoveValue values
    RemoveValue val -> [AddValue val]
    AddValue val -> [RemoveValue val]
  
-- | Generates tag operation for dimensions. Used when creating a box
-- to update the dimension tags. 
dimensionTagOps :: Dimension  -> [Tag'Operation]
dimensionTagOps dim = [dshow 'l' dLength, dshow 'w' dWidth, dshow 'h' dHeight]
  where dshow c f = ( pack $ '\'' : c : []
                    , SetValues [tshow (floor $ 100 * f dim)]
                    )

-- | Extract new dimensions from tags
-- use given dimension for missing elements
-- return Nothing, if nothing has changed, allowing optimisation upstream
extractDimensions :: Dimension -> Tags -> Maybe Dimension
extractDimensions dim tags = case (go "'l", go "'w", go "'h") of
  (Nothing, Nothing, Nothing) -> Nothing
  (l, w, h) -> Just dim { dLength = fromMaybe (dLength dim) l
                                , dWidth = fromMaybe (dWidth dim) w
                                , dHeight = fromMaybe (dHeight dim) h
                                }
  where go prefix = case traverse readMay $ maybe [] (Set.toList) (Map.lookup prefix tags) of
          Nothing -> Nothing
          Just values -> Just $ sum values /100

-- | Change the dimension of the box according to its tag
updateDimFromTags :: Box s -> Box s
updateDimFromTags box = case extractDimensions (_boxDim box) (boxTags box) of
  Nothing -> box
  Just dim -> box { _boxDim = dim  }

 
-- | Assign a box to shelf regardless of if there is enough space
-- left or not.
-- For a "real" move see moveBox
assignShelf :: (Box' box,  Shelf' shelf) => Maybe (shelf s) -> box s -> WH (Box s) s
assignShelf s box0 = do
    box <- findBox box0
    oldShelfM <- traverse findShelf (boxShelf box)
    newShelfM <- traverse findShelf s

    -- if box belong to a shelf
    -- we need to update the shelf to remove the link to the box
    if (oldShelfM /= newShelfM) 
    then do
      mapM_ (unlinkBox (boxId box)) oldShelfM
      mapM_ (linkBox (boxId box)) newShelfM
      updateBox (\box_ -> box_ { boxShelf = shelfId `fmap` s }) box
    else
      return box

-- | Unlink a box from a shelf without
-- checking anything. Shoudn't be exported
unlinkBox :: BoxId s -> Shelf s -> WH () s
unlinkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = delete box boxes

      shelf' = shelf { _shelfBoxes = boxes' }
  _ <- updateShelf (const shelf') shelf'
  return ()

-- | link a box from a shelf without
-- checking anything. Shoudn't be exported
linkBox :: BoxId s -> Shelf s -> WH () s
linkBox box shelf = do
  let boxes = _shelfBoxes shelf
      boxes' = boxes |> box  

      shelf' = shelf { _shelfBoxes = boxes' }
  _ <- updateShelf (const shelf') shelf'
  return ()

deleteBoxes :: [Box s] -> WH [Box s] s
deleteBoxes boxes_ = do
  let boxIds = Set.fromList $ map boxId boxes_
  deleted <- forM boxes_ $ \box -> do
                oldShelfM <- traverse findShelf (boxShelf box)
                mapM_ (unlinkBox $ boxId box) oldShelfM
                return box
  wh <- get
  put wh { boxMap = Map.filter (not . null)
                 $ fmap (filter (`notElem` boxIds)) (boxMap wh) 
         }
  return deleted
    


deleteShelf :: ShelfId s -> WH () s
deleteShelf shelfId = do
  findBoxByShelf shelfId >>= deleteBoxes
  warehouse <- get
  put warehouse { shelves = filter (/= shelfId) $ shelves warehouse }
  
clampTilingMode :: Maybe Int -> Maybe (Int, Int) -> Maybe Int -> TilingMode -> TilingMode
clampTilingMode maxLM' mWM' maxHM' mode' = fst $ go maxLM' mWM' maxHM' mode' where
  go maxLM mWM maxHM mode = 
    case mode of 
       Regular hmany -> mk Regular hmany
       Diagonal hmany d -> mk (flip Diagonal d) hmany
       TilingCombo Horizontal m1 m2 -> let
                    -- in horizontal mode, only "uses" length param
                    (m1', (maxLM', _, _)) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM' mWM maxHM m2
                    in (TilingCombo Horizontal m1' m2', next)
       TilingCombo Vertical m1 m2 -> let
                    -- in vertical mode, only "uses" length param
                    (m1', (_, _, maxHM')) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM mWM maxHM' m2
                    in (TilingCombo Vertical m1' m2', next)
       TilingCombo Depth m1 m2 -> let
                    -- in vertical mode, only "uses" length param
                    (m1', (_, mWM',_)) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM mWM' maxHM m2
                    in (TilingCombo Depth m1' m2', next)
    where clamp (HowMany _ nl nw nh) = let
                (minW, maxW) = unzip mWM
                hwmany = mkHowMany (minMaybe maxLM nl)
                                   (maxMaybe minW (minMaybe maxW nw))
                                   (minMaybe maxHM nh)
                nextParam = (fmap (\maxL -> maxL - perLength hwmany) maxLM
                            , fmap (\(_,maxW) -> (0, maxW - perDepth hwmany )) mWM
                            , fmap (\maxH -> maxH - perHeight hwmany) maxHM
                            )
                in (hwmany, nextParam)
          mk constr hw = let
             (hw', nextParams) = clamp hw
             in (constr hw', nextParams)
          minMaybe minm x = fromMaybe x  $ fmap (min x) minm
          maxMaybe maxm x = fromMaybe x  $ fmap (max x) maxm

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


-- | * test
{-
dx = Dimension 192 100 145
dn = Dimension 160 100 145
box = Dimension 47 39 85
os = [tiltedForward, tiltedFR]
-}


-- | How many inner rectangle can fit in the outer one ?
-- The outer min corresponds to the maximum of the n-1 box
-- This allows to set for example a maximum height for the bottom the last box
-- this is the maximum height a operator can reach the box
-- and the actual height of the shelf or ceiling, the physical limit.
--
--   max ----------------------
--           2    X   
--           2    X
--   min ____2____2___________
--           1    2
--           1    1
--           1    1
--
-- X is accepted even though it fit within max
-- but starts after min
--
howMany :: Dimension --  ^ Outer min
        -> Dimension -- ^ Out max
        -> Dimension --  ^ Inner
        -> HowMany
howMany (Dimension l0 w0 h0) (Dimension l w h) (Dimension lb wb hb) =
        mkHowMany ( fit l0 l lb)
                  ( fit w0 w wb)
                  ( fit h0 h hb)
        where
        fit d0 d1 db = 
          let d = min (d0+db) d1
          in floor (max 0 (d-0) /(db+0))


-- | Find how many boxes fits using a "diagnal trick", when one box is rotated the other along a diagoral.
-- For example
--    1477
--    14 8
--    2558
--    2 69
--    3369
--  3, 5 and 7 box are rotated down allowing f
howManyWithDiagonal :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithDiagonal minOuter outer inner@(Dimension lb _ hb) | lb == hb =
  Regular $ howMany minOuter outer inner
howManyWithDiagonal minOuter outer@(Dimension l _ h) inner@(Dimension lb _ hb) | lb < hb = 
  let normal@(HowMany _ _ wn hn) = howMany minOuter outer inner
      fit d db = floor (max 0 (d-0) /(db+0))
      -- how many extra can we squeeze horizontally
      -- if we turn the box
      diff = hb - lb -- >0 
      -- check how many squares actually fit height wise
      minSquareNumberV = max 1 do  (diff - 1 + fromIntegral (hn+1) * hb - h) `fit` diff
      maxSquareWidth = case minSquareNumberV of
                       0 -> hn 
                       _ -> (hn + 1) `div` minSquareNumberV
      nForDiag n =
         --  | = =  | = =
         --
         --  = = |  = = |     1 square x 2
         --  = | =  = | =
         --  | = =  | = =
         --
         let squareL = fromIntegral (n-1)*lb+hb
             squareH = fromIntegral (n-1)*hb+lb
             sqNL = fit l squareL
             sqNH = fit h squareH
             leftL = l - squareL * fromIntegral sqNL
             leftH = h - squareH * fromIntegral sqNH
             mb = hb -- max lb hb
             -- find how many row/column can we use
             -- in a partial square
             -- We use mb because the first row/column
             -- will use both orientation for one of the box
             leftOver remaining b =
              if remaining < mb
              then 0
              else -- traceShow ("Remaining", remaining, remaining-mb, "b" , b)
                   fit (remaining -  mb) b +1
              
         in Diagonal (mkHowMany (sqNL * n + leftOver leftL lb)
                                 wn
                                 (sqNH * n + leftOver leftH hb)
                      )
                      n
   -- in case (min maxSquareNumberH minSquareNumberV , [max 2 minSquareWidth .. maxSquareWidth]) of
   in case (minSquareNumberV, [2 .. maxSquareWidth]) of
         (0, _) -> Regular normal
         (_, []) -> Regular normal
         (_, ds) -> let options = map nForDiag ds
                        bests = (Regular normal): options
                    in if outer /= minOuter
                       then Regular normal
                       else case bests of
                        [] -> error "Shouldn't happen"
                        _ -> minimumEx bests
howManyWithDiagonal minOuter outer inner = 
  rotateTM $ howManyWithDiagonal (r minOuter) (r outer) (r inner) where
  r = rotate tiltedRight

-- | Find how many  boxes  fit using two regular tiling as 
-- For example
--  44 55 66 7 8
--  11 22 33 7 8
-- At the moment tries different diagonol layout.
howManyWithSplitH :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithSplitH minOuter outer inner =  let
  tmode = Regular $ howMany minOuter outer inner 
  currentTotal = tmTotal tmode
  tries = [ TilingCombo Horizontal leftMode rightMode
          | i <- [tmLength tmode `div` 2 .. tmLength tmode ]
          -- we only need to try half of the solution, because
          -- the other will be tested when we try the rotated box
          , let leftMode = clampTilingMode (Just i) Nothing Nothing tmode
          , let bbox = tmBoundingBox leftMode inner
          , let used = invert $ Dimension (dLength bbox) 0 0
          , let rightMode =  howManyWithDiagonal (minOuter <> used) (outer <> used) (rotate tiltedRight inner)
          , tmTotal rightMode + tmTotal leftMode > currentTotal
          ]
  in minimumEx (tmode: tries)
howManyWithSplitV :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithSplitV minOuter outer inner = let
  rot = rotate tiltedRight 
  in rotateTM $ howManyWithSplitH (rot minOuter) (rot outer) (rot inner)
  
rotateTM (Regular hmany) = Regular (rotateH hmany)
rotateTM (Diagonal hmany n) = Diagonal (rotateH hmany) n
rotateTM (TilingCombo dir m1 m2) = let
         dir' = case dir of
                 Horizontal -> Vertical
                 Vertical -> Horizontal
                 Depth -> Depth
         in TilingCombo dir' (rotateTM m1) (rotateTM m2)
rotateH (HowMany n l w h) = HowMany n h w l
  
-- | Given a box and a tiling mode, returns the bounding box
tmBoundingBox :: TilingMode -> Dimension -> Dimension
tmBoundingBox (Regular (HowMany _ l w h)) (Dimension lb wb hb) =
  Dimension (fromIntegral l * lb)
            (fromIntegral w * wb)
            (fromIntegral h * hb)
tmBoundingBox (Diagonal (HowMany _ l w h) d) box =
  fst $ indexToOffsetDiag box d (l+1, w+1, h+1)
tmBoundingBox (TilingCombo dir m1 m2) box = let
  Dimension l1 w1 h1 = tmBoundingBox m1 box
  Dimension l2 w2 h2 = tmBoundingBox m2 (rotate tiltedRight box)
  in case dir of
       Horizontal -> Dimension ((+) l1 l2) (max w1 w2) (max h1 h2)
       Depth -> Dimension (max l1 l2) ((+) w1 w2) (max h1 h2)
       Vertical -> Dimension (max l1 l2) (max w1 w2) ((+) h1 h2)



                 















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



d0, r :: Dimension
d0 = Dimension 0 0 0
r = Dimension 13 1 10
-- | Computes position and orientation of a box within a "Diagonal" pattern
-- see `howManyWithDiagonal`
indexToOffsetDiag :: Dimension -> Int -> (Int, Int, Int) -> (Dimension, Bool)
indexToOffsetDiag (Dimension l w h) diagSize (il, iw, ih) =
  let (lq, lr) = il `divMod`  diagSize
      (hq, hr) = ih `divMod` diagSize
      --    
      --    b   X|    X
      --      X  |  X c
      --    X   a|X  
      -- lq  0    1     2
      --    
      -- X) turned
      -- a) (il, ih) = (2,0)
      --    (lq, lr) = (0,2) 1 before
      --    (hq, hr) = (0,0) 0 below
      --    
      --
      -- c) (il, ih) = (5,1)
      --    (lq, lr) = (1,2) 2 before
      --    (hq, hr) = (0,1) 0 below
      (turnedBefore, turnedBelow, turned) =
        if lr > hr
        then -- left of the diagonal turnedBelow +1
          (lq+1, hq, False)
        else if lr ==  hr -- on the diagnoal
        then
          (lq, hq, True)
        else -- right of diagonal
          (lq, hq+1, False)
  in  (Dimension (fromIntegral (il-turnedBefore) * l + fromIntegral turnedBefore*h)
                 (fromIntegral iw * w)
                 (fromIntegral (ih - turnedBelow) * h + fromIntegral turnedBelow*l)
      , turned
      )
          

data SortBoxes = SortBoxes | DontSortBoxes
     deriving (Eq, Ord, Show)
-- Try to Move a block of boxes  into a block of shelves.
-- Boxes are move in sequence and and try to fill shelve
-- in sequence. If they are not enough space the left boxes
-- are returned.
moveBoxes :: (Box' box , Shelf' shelf) => ExitMode -> PartitionMode -> SortBoxes -> [box s] -> [shelf s] -> WH [Box s] s

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
        
  lefts_ <- forM layers $ \layer -> do
    let groups = groupSimilar _boxDim layer
    -- forM groups $ \(SimilarBy dim _ boxes) -> traceShowM ("  GROUP", dim, 1 + length boxes)
    -- traceShowM ("GRoups", length groups, map (\(SimilarBy dim g1 g ) -> (show $ length g + 1, show . _roundDim $ dim )) groups)
    lefts' <- mapM (\g -> moveSimilarBoxes exitMode partitionMode g ss) groups
    return $ concatMap unSimilar $ catMaybes lefts'
  return $ concat lefts_

_roundDim :: Dimension -> [Int]
_roundDim (Dimension l w h) = map (round . (*100)) [l,w,h]

 
-- | Move boxes of similar size to the given shelf if possible
moveSimilarBoxes :: (Shelf' shelf) => ExitMode -> PartitionMode -> SimilarBoxes s -> [shelf s] -> WH (Maybe (SimilarBoxes s)) s
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
assignBoxesToPositions :: Slices k (Shelf s, Position) -> SimilarBoxes s -> WH (Maybe (SimilarBoxes s)) s
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
  return $ dropSimilar (length boxes - length leftOver) simBoxes
  
boxRank :: Box s -> (Text, Int, Text, Int)
boxRank box = ( boxStyle box , boxStylePriority box, boxContent box, boxContentPriority box)
-- | get the priority of a box. At the moment it is extracted from the tag.
-- we might set it as an attribute to speed things up
-- extract number from tag
boxContentPriority :: Box s -> Int
boxContentPriority box = p where (_, _, p) = boxPriorities box

boxStylePriority  :: Box s -> Int
boxStylePriority  box = p where (_,p,_)  = boxPriorities box

-- | Same as boxPriority but used before grouping box by styles
-- look at @n in tags
boxGlobalPriority  :: Box s -> Int
boxGlobalPriority  box = p where (p, _, _) = boxPriorities box


-- | Remove boxes for shelves and rearrange
-- shelves before doing any move
-- aroundArrangement  :: WH a -> WH a
aroundArrangement :: (Shelf' shelf, Box' box, Box' box2)
                  => AddOldBoxes 
                  -> (forall b . Box' b =>  [b s] -> [shelf s] -> WH [box2 s] s)
                -> [box s] -> [shelf s] -> WH [box2 s] s
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

    left <- arrangement boxes shelves
    s0 <- defaultShelf
    void $ mapM (assignShelf (Just s0)) left
    return left









updateBox :: (Box' box) =>  (Box s ->  Box s) -> box s-> WH (Box s) s
updateBox f box0 = do
    box <- findBox box0
    let box' = f box
    lift $ writeSTRef (getRef box') box'
    return box'

updateShelf :: (Shelf' shelf) => (Shelf s -> Shelf s ) -> shelf s -> WH (Shelf s) s
updateShelf f s =  do
    shelf <- findShelf s
    let shelf' = f shelf
    lift $ writeSTRef (getRef shelf') shelf'
    return shelf'


updateShelfByName :: (Shelf s -> Shelf s) -> Text -> WH [Shelf s] s
updateShelfByName f n = findShelfBySelector (Selector (NameMatches [MatchFull n]) [] ) >>= mapM (updateShelf f)


-- | Add or remove the given tags to the give box
updateBoxTags' :: [Tag'Operation] -> Box s -> Box s
updateBoxTags' [] box = box -- no needed but faster, because we don't have to destruct and 
updateBoxTags' tag'ops box = case modifyTags tag'ops (boxTags box) of
  Nothing -> box
  Just new -> let newContent = getTagValuem new "'content"
                  -- replace the content virtual tag
                  cleanTag = case newContent of
                                  Just cont -> \t -> (Map.withoutKeys t (Set.fromList ["'content", "'" <> boxContent box]))
                                              <> (Map.singleton ("'" <> cont) mempty)
                                  Nothing -> id
              in updateDimFromTags box { boxTags = cleanTag new
                                    , boxPriorities = extractPriorities new (boxPriorities box)
                                    , boxBreak = extractBoxBreak new
                                    , boxContent = fromMaybe (boxContent box) newContent
                                    }

updateShelfTags' :: [Tag'Operation] -> Shelf s -> Shelf s
updateShelfTags' [] shelf = shelf -- no needed but faster, because we don't have to destruct and 
updateShelfTags' tag'ops shelf = case modifyTags tag'ops (shelfTag shelf) of
  Nothing -> shelf
  Just new -> updateCeiling $ shelf { shelfTag = new }
  where updateCeiling s =
          let heightMax  = dHeight (maxDim s)
              reduce extra dim = if dHeight dim <= extra
                                 then -- too small, we can't set the height to 0
                                      -- so we need to set width to 0 to make it
                                      -- unusable
                                    dim { dWidth = 0, dHeight = 1 }
                                 else
                                    dim {dHeight = dHeight dim - extra }
          in case getTagValuem s "ceiling" >>= readMay of 
              Just ceiling | ceiling < heightMax + bottomOffset s
                    ->   let extra  = heightMax + bottomOffset s - ceiling
                         in  s { shelfTag = Map.insert "'tooHigh" mempty (shelfTag s) 
                               , maxDim = reduce extra $ maxDim s
                               , minDim = reduce extra $ minDim s
                               }
              _ -> s

-- | Update the value associateda to a tag operation. Return Nothing if the tag needs to be destroyed
applyTagOperation :: TagOperation -> (Set Text) -> Maybe (Set Text)
applyTagOperation RemoveTag _ = Nothing 
applyTagOperation SetTag olds = Just olds
applyTagOperation (SetValues news) _ = Just $ Set.fromList news
applyTagOperation (AddValue value) olds = Just $ Set.insert value olds 
applyTagOperation (RemoveValue value) olds = Just $ Set.delete value olds

applyTagOperations :: [TagOperation] -> (Set Text) -> Maybe (Set Text)
applyTagOperations tag'ops tags = foldM (flip applyTagOperation) tags tag'ops


-- | Apply tag operations to a set of tags. Return nothing
-- if nothing has changed. Knowing nothing has changed should
-- allow some optimization upstream
modifyTags :: [Tag'Operation] -> Tags -> Maybe Tags
modifyTags [] __tags = Nothing
modifyTags tag'ops tags = Just $ merge  opsOnly tagsOnly tagsAndOp tag'opsMap tags where
    tagsOnly = preserveMissing
    opsOnly = mapMaybeMissing $ \_ ops -> applyTagOperations ops mempty
    tagsAndOp = zipWithMaybeMatched $ \_ -> applyTagOperations
    -- tagoperation should be applied in order so we should be able to put them in a map
    -- however every key works independently of the othere, so at least
    -- as we group_ each operation by key and respect the order, this should be
    tag'opsMap :: Map.Map Text [TagOperation]
    tag'opsMap = Map.fromListWith (<>)  (map (fmap (:[])) tag'ops)

updateBoxTags :: [Tag'Operation] -> Box s -> Int -> WH (Box s) s
updateBoxTags tags0 box index = do
  -- remove '''
  tags1 <- mapM (mapM $ mapM (expandAttribute box index)) tags0
       --                 ^--  each value in Operation
       --     ^    ^-- each value of the TagOperation
       --     +------ snd of the (,)
  let tags = [ (replaceSlashes tag, fmap replaceSlashes values )
             | (tag, values) <- tags1
             ]
  updateBox (updateBoxTags' tags) box

updateShelfTags :: [Tag'Operation] -> Shelf s -> WH (Shelf s) s
updateShelfTags tags0 shelf =  do
  let tags = [ (replaceSlashes tag, fmap replaceSlashes values )
             | (tag, values) <- tags0
             ]
  updateShelf (updateShelfTags' tags) shelf

boxStyleAndContent :: Box s -> Text
boxStyleAndContent box = case boxContent box of
  "" -> boxStyle box
  c -> boxStyle box ++ "-" ++ c
  
-- | Box coordinate as if the shelf was full of this box
-- give the offest divide by the dimension of the box + 1
boxCoordinate :: Box s -> Dimension
boxCoordinate = fst . boxPosition
                     
-- | Box coordinate + left over                   
boxPosition :: Box s -> (Dimension, Dimension)
boxPosition box  = let
  (Dimension ol ow oh) = boxOffset box
  (Dimension l w h) = boxDim box
  go o d = let q = fromIntegral $ floor (o / d)
               r = o - q * d
           in (q+1,r)
  (lq,lr) = go ol l
  (wq,wr) = go ow w
  (hq,hr) = go oh h
  in (Dimension lq wq hq, Dimension lr wr hr)
  
boxPositionSpec :: Box s -> Text
boxPositionSpec box = let
  (coord, offset) = boxPosition box
  coordString = case coord of
            Dimension 0 0 0 -> ""
            Dimension l w h -> printf "%.f:%.f:%.f" l w h
  offsetString  = case offset of
                  Dimension 0 0 0 -> ""
                  Dimension l w h-> printf "+%.f+%.f+%.f" l w h
  in showOrientation' (orientation box) <> pack coordString <> pack offsetString


-- | Box attribute can be used to create new tag
-- example, /pending,#previous=$shelfname on a
-- will add the tag previous=pending to all items in the pending shelf
expandAttribute :: Box s -> Int -> Text -> WH Text s
expandAttribute box index toExpand = maybe (return toExpand) (\f -> f box index) (expandAttributeMaybe toExpand)


-- | Workhorse for expandAttribute. The difference is it actually doesn't need a box
-- to know if it needs expansion or not
-- If an attribute is found, we can safely call expandAttribute (recursively), as we are
-- only interested in doest in need expansion or not
expandAttributeMaybe :: Text -> Maybe (Box s -> Int -> WH Text s)
expandAttributeMaybe text = let
  wrap :: Box s -> Int -> (Text -> Box s -> Int -> WH Text s) -> Text -> WH Text s
  wrap box index f subtext =
    case (uncons subtext, T.breakOn "}" subtext) of
      -- (_, "") -> f subtext box
      (Just ('{', _) , (key, leftOver)) -> (<> drop 1 leftOver) <$> f key box index
      _ -> f subtext box index
  in case splitOn "$" text of
     prefix:segments -> Just $ \box i -> do
      expandeds <- mapM (wrap box i expandAttribute') segments
      return $ concat (prefix : expandeds)
     _ -> Nothing
expandAttribute' :: Text -> Box s -> Int -> WH Text s
expandAttribute' "" = \_ _ -> return "$"
expandAttribute' (uncons -> Just ('{', prop)) = \box _i -> do
  shelfId <- case boxShelf box of
              Just s -> return s
              Nothing -> defaultShelf
  shelf <- findShelf shelfId
  return $ either tshow id $ expandIntrinsic prop box shelf
  

expandAttribute' (stripPrefix "[" -> Just xs'') | (pat', uncons -> Just (_,xs'))<- break (== ']') xs'' = \box i -> do
                               ex <- expandAttribute box i xs'
                               pat <- expandAttribute box i pat'
                               let (tag, evaluator) = parseEvaluator pat
                               return $ maybe ex (<> ex) (evaluator $ getTagValuesWithPresence box tag)
expandAttribute' (stripPrefix "/" -> Just xs'') | (pat', uncons -> Just (_,xs'))<- break (== '/') xs'' = \box i -> do
                               ex <- expandAttribute box i xs'
                               pat <- expandAttribute box i pat'
                               shelfId <- case boxShelf box of
                                           Just s -> return s
                                           Nothing -> defaultShelf
                               shelf <- findShelf shelfId
                               let (tag, evaluator) = parseEvaluator pat
                               return $ maybe ex (<> ex) (evaluator $ getTagValuesWithPresence shelf tag)
                               
-- get the rank of the tag value
expandAttribute' (stripStatFunction -> Just (stat, arg, prop, xs))  = \box i -> 
  case stat of
    "rank" -> expandStatistic valueRank arg box prop xs
    "index" -> expandStatistic valueIndex arg box prop xs
    "ago" ->  do
        maxDate <- gets whDay
        valuem <- listToMaybe <$> expandOrdkey box Nothing prop
        stats <- propertyStatsFor prop
        let dates = keys (valueIndex stats)
        return $ case (readMay =<< minimumMay dates, readMay =<< valuem) of
          (Just minDate, Just currentDate) -> let
            daysAgo = diffDays maxDate currentDate
            range = diffDays maxDate minDate
            d = case arg of
              Just ('-', 0) -> -- normalize a year to n
                     (daysAgo `div` 365) + 1
              Just ('-', n) -> -- normalize range to n
                     min (daysAgo * fromIntegral n `div` range + 1) (fromIntegral n)
              Just ('%', 0) -> -- normalize a year to n
                     (daysAgo -1) `mod` 365 + 1
              Just ('%', n) -> -- normalize a year to n
                     (daysAgo -1) * fromIntegral n `mod` range + 1
              Just ('^', 0) -> case daysAgo of -- log day, week etc
                d' | d' <= 7 -> 1 -- last week
                d' | d' <= 31 -> 2 -- last month
                d' | d' <= 91 -> 3 -- last quarter
                d' | d' <= 183 -> 4 -- last 6 months
                d' | d' <= 365 -> 5 -- last year
                d' | d' <= 3*365 -> 6 -- last 3 years
                _ -> 7
              Just ('^', n) -> let -- log 
                       daysAgo' = fromIntegral daysAgo
                       n' = fromIntegral n
                       range' = fromIntegral range
                       lambda = range' / log n' :: Double
                       in min (fromIntegral n) $ round $ exp (lambda *  daysAgo')
              _ -> daysAgo
            in tshow (d :: Integer) <> xs
          _ -> "<not a stat>" <> xs
    "n" -> do
          let v = evalArg arg i
              format = if null propText  then "%d" else unpack propText
          return $ (pack $ printf format v) <> xs
    "select" -> do
             let e = case splitOn "|" propText of
                          [] -> tshow i
                          values -> values List.!! (min (length values) (evalArg arg i) - 1)
             return $ e <> xs
    "cycle" -> do
                 let e = case splitOn "|" propText of
                       [] -> tshow i
                       values -> values List.!! mod (evalArg arg i - 1) (length values)
                 return $ e <> xs

    _ -> return $ "<not a stat> xs"
    where evalArg arg i = case arg of
                                 Just ('-', n) -> i -n
                                 Just ('+', n) -> i + n
                                 Just ('%', n) -> (i - 1) `mod` n + 1
                                 Just ('*', n) -> i * n
                                 Just ('/', n) -> i `div` n
                                 Just ('^', n) -> min i n
                                 _ -> i
          propText = case prop of
                      OrdTag tag -> tag
                      OrdAttribute att -> att


expandAttribute' text = \_ _ -> return text

expandIntrinsic :: Text -> Box s -> Shelf s -> Either  Int Text
expandIntrinsic prop0 box shelf = let
  (prop, evaluator) = parseEvaluator prop0
  in fmap (fromMaybe "" . evaluator . pure) $ expandIntrinsic' prop box shelf
expandIntrinsic' :: Text -> Box s -> Shelf s -> Either  Int Text
expandIntrinsic' "shelfname" box shelf = do
  case boxShelf box of
    Nothing -> Right ""
    Just _ -> Right $ shelfName shelf
expandIntrinsic' "shelftags" box shelf =do
  case boxShelf box of
    Nothing -> Right ""
    Just _ -> Right $ (intercalate "#" . flattenTags $ shelfTag shelf)
expandIntrinsic' "fit" box shelf =do
  case boxShelf box of
    Nothing -> Right ""
    Just _ -> 
      let   Dimension xn yn zn = minDim shelf
            Dimension xx yx zx = maxDim shelf
            -- Dimension xx yx zx = maxDim shelf
            Dimension l w h = boxDim box
            Dimension ox oy oz = boxOffset box
            fit = case ( (ox+l) > xn || (oy+w) > yn || (oz+h) > zn  
                          , (ox+l) > xx || (oy+w) > yx || (oz+h) > zx
                          ) of
                      (_, True) -> "out"
                      (True, False) -> "tight"
                      (False, False) -> "fit"
      in Right $ fit
expandIntrinsic' "ol" box _shelf =let (Dimension ol _ _ ) = boxCoordinate box in  Left $ round ol
expandIntrinsic' "ow" box _shelf =let (Dimension _ ow _ ) = boxCoordinate box in  Left $ round ow
expandIntrinsic' "oh" box _shelf =let (Dimension _ _ oh ) = boxCoordinate box in  Left $ round oh
expandIntrinsic' "@" box _shelf =let (global, style, content) = boxPriorities box in  Right $ tshow content <> "@" <> tshow style <> "@" <> tshow global
expandIntrinsic' "@content" box _shelf = Left $ boxContentPriority box
expandIntrinsic' "@style" box _shelf = Left $ boxStylePriority box
expandIntrinsic' "@global" box _shelf = Left $ boxGlobalPriority box
expandIntrinsic' "style" box _shelf = Right $ boxStyle box
expandIntrinsic' "content" box _shelf = Right $ boxContent box
expandIntrinsic' "boxname" box _shelf = Right $ boxStyleAndContent box
expandIntrinsic' "coordinate" box _shelf = let (Dimension ol ow oh) = boxCoordinate box
                                               roundi i = (round i) :: Int
                                           in  Right $ pack $ printf "%d:%d:%d" (roundi ol) (roundi ow) (roundi oh)
expandIntrinsic' "offset" box _shelf = Right $ printDim $ boxOffset box
expandIntrinsic' "dimension" box _shelf = Right $ printDim $ _boxDim box
expandIntrinsic' "orientation" box _shelf = Right $ showOrientation (orientation box)
expandIntrinsic' prop _box _shelf =  Right $ "${" <> prop <> "}"


expandStatistic :: (PropertyStats -> Map Text Int) -> Maybe (Char, Int) -> Box s -> OrderingKey -> Text -> WH Text s
expandStatistic fn arg box prop xs = do
  values <- expandOrdkey box Nothing prop
  stats <- propertyStatsFor prop
  return $ case values of
    [] -> xs
    (value:_) -> let
      adjust i = case arg of
        Just ('-', n) -> min i n
        Just ('%', n) -> (i-1) `mod` n + 1
        Just ('^', n) -> round $ fromIntegral i / fromIntegral (totalCount stats) * fromIntegral n
        _ -> i
      in maybe xs (\i -> tshow (adjust i) <> xs) (lookup value $ fn stats) 

-- | Expand Tag or Attribute. Fetch the shelf if not provided
expandOrdkey :: Box s -> Maybe (Shelf s) ->  OrderingKey -> WH [Text] s
expandOrdkey box shelfm key = do
   case key of
    OrdTag tag -> return$ getTagValues box tag
    OrdAttribute att -> do
      shelf <- maybe (defaultShelf >>= findShelf) return shelfm
      let e = expandIntrinsic att box shelf
      return [either tshow id e]

replaceSlash '/' = '\''
replaceSlash c  = c
replaceSlashes :: Text -> Text 
replaceSlashes = omap replaceSlash
   
defaultPriority :: Int
defaultPriority = 100
defaultPriorities = (defaultPriority, defaultPriority, defaultPriority)

-- | Parse stat function name and its parameter
-- example
--  - $rank
--  - $rank-100 - normalize to 100
--  - $rank^100 - cut to 99 and everything above = 100
--  - $rank%100 - modulo 100

stripStatFunction :: Text --  ^ text  to parse
                  -> Maybe (Text -- stat function
                           , Maybe (Char, Int) -- operator number
                           , OrderingKey  -- Tag or Attribute
                           , Text) -- left over
stripStatFunction xs = either (const Nothing) Just $  P.parse parser  (unpack xs) xs where
  parser = do
        stat <- asum $ map P.string  ["rank", "index", "ago", "n", "select", "cycle"]
        opM <- P.optionMaybe  do
                op <- P.oneOf "-+*/%^"
                Just n <- readMay <$> P.many1 P.digit
                return (op, n)
        att <- parseTag <|> parseAttribute
        leftOver <- P.many P.anyChar
        return $ (pack stat, opM, att, pack leftOver)
  parseTag = OrdTag . pack <$> do P.char '[' >> P.many (P.noneOf "]") <* P.char ']'
  parseAttribute = OrdAttribute . pack <$> do P.char '{' >> P.many1 (P.noneOf "}") --  <* P.char '}'
  -- The trailing } has been stripped already
                  
    
_stripStatFunction xs  = do
  (prefix, drop 1 -> xs') <- breakm (== '[') xs -- rank-100 [name]
  (prop, drop 1 -> xs'') <- breakm (==']') xs'
  case breakm (not . isLetter) prefix of
    Just (pre, uncons -> Just (op, argText)) | Just arg <- readMay argText -> Just (pre, Just (op, arg), prop, xs'')
    _ -> Just (prefix, Nothing, prop, xs'')
  where breakm cond text = 
          case break cond text of
            (_,"") -> Nothing
            a -> Just a


-- | Syntax tag[?]
parseEvaluator :: Text -> (Text, [Text] -> Maybe Text)
parseEvaluator tag0 | Right (tag, thenValue, elseValue) <- P.parse parser (unpack tag0) tag0 = 
  ( tag 
  , \case
      [] -> elseValue
      _ -> thenValue <|> Just tag
  )
  where parser = do
          tag <- P.many1 $ P.noneOf "?"
          P.char '?'
          thenValue <- P.optionMaybe (P.many1 $ P.noneOf ":")
          elseValue <- P.optionMaybe do
                     P.char ':' >> (P.many1 $ P.anyChar)
          return (pack tag,  pack <$> thenValue, pack <$> elseValue)
-- parse value:start:end
parseEvaluator tag0 | Right (tag, startm, endm) <- P.parse parser (unpack tag0) tag0 =
  let sub = removePrefix . removeSuffix
      removePrefix t = case (startm >>= readMay, startm) of
                            (Just n, _) -> if | n > 0 -> drop n t
                                              | n == 0 -> t
                                              | otherwise -> drop (length t + n) t
                            (Nothing, Just pre) ->  snd $ T.breakOnEnd pre t
                            _ -> t
      removeSuffix t = case (endm >>= readMay, endm) of
                            (Just n, _) -> if | n > 0 -> take n t
                                              | n == 0 -> t
                                              | otherwise -> take (length t + n) t
                            (Nothing, Just suff) ->  fst $ T.breakOn suff t
                            _ -> t

  in ( tag
     , \case
        [] -> Nothing
        values -> Just $ intercalate ";" $ map sub values
     )
  where parser :: P.Parser (Text, Maybe Text, Maybe Text)
        parser = do
          tag <- P.many1 $ P.noneOf ":"
          P.char ':'
          startm <- P.optionMaybe (P.many1 $ P.noneOf ":")
          endm <- P.optionMaybe do
            P.char ':' >> (P.many1 $ P.anyChar)
          return (pack tag, pack <$> startm, pack <$> endm)
parseEvaluator tag0 | Right (tag, format) <- P.parse parser (unpack tag0) tag0 =
  let f ::  Int -> Text
      f =  pack . printf format
  in ( pack tag
     , \case
        [] -> Nothing
        values -> Just . f $ sum $ mapMaybe readMay values
     )
  where parser :: P.Parser (String, String)
        parser = do
           tag <-  P.many1 $ P.noneOf "%"
           format <- P.char '%' >> do P.many P.anyChar
           return (tag, '%':format)
parseEvaluator tag =
  ( tag
  , \case 
     [] -> Nothing
     values -> Just $ intercalate ";" values
  )
        

printDim :: Dimension -> Text
printDim  (Dimension l w h) = pack $ printf "%0.1fx%0.1fx%0.1f" l w h
-- | Convert a set of tags to prioties
-- bare number = content priority, 
-- @number style priority
-- @@number global priority
extractPriorities :: Tags -> (Int, Int, Int) -> (Int, Int, Int)
extractPriorities tags (g0, s0, c0) = let
  go key p0 = fromMaybe p0 $ extractPriority key tags 
  in (go "@global" g0 , go "@style" s0, go "@content" c0 )



extractPriority :: Text -> Tags -> Maybe Int
extractPriority key tags = do
  set <- Map.lookup key tags
  case mapMaybe readMay (toList set) of
    ns@(_:_) -> Just $ sum ns
    _ -> Nothing


extractBoxBreak :: Tags -> Maybe BoxBreak
extractBoxBreak tags = case maybe [] (Set.toList) (Map.lookup "@start" tags) of
  ["new-slot"] -> Just StartNewSlot
  ["new-slice"] -> Just StartNewSlice 
  ["new-shelf"] ->Just StartNewShelf 
  _ -> Nothing

-- * Misc 
-- | reorder box so they are ordered by column across all
-- the given shelves.
-- sortAccross :: Shelf' shelf => [shelf s] -> WH [Box s] s
-- sortAccross ss = do
--     -- first we need to remove the boxes from their current location
--     boxes <- concat `fmap` mapM findBoxByShelf ss
--     let nothing = headEx $ Nothing : map Just ss -- trick to typecheck
--     mapM (assignShelf  nothing) boxes
--     left <- iteration ss boxes
--     s0 <- defaultShelf
--     mapM (assignShelf  (Just s0)) left
--     return (error "FIXME") --  left
--     -- Similar is not

--     where iteration ss oldLeft = do
--               left <- foldM (\left s -> undefined) -- fillShelf ExitOnTop s (Similar left)) oldLeft  ss
--               if Prelude.length left == Prelude.length oldLeft
--               then return left
--               else iteration ss left

usedDepth :: Shelf' shelf => shelf s -> WH (Text, Double) s
usedDepth s = do
  boxes <- findBoxByShelf s
  return $ List.maximumBy (comparing snd) (("<empty>",0)
                         :[(boxStyle box, dWidth (boxCorner box))
                   | box <- boxes
                   ])




-- * Denormalizing 
--
shelfBoxes :: WH [(Shelf s, Box s)] s
shelfBoxes = do
    ss <- mapM findShelf =<< (toList <$> gets shelves)
    sbsS <- mapM (\s -> do bs <- findBoxByShelf s ; return (s, bs)) ss

    return [(s, box) | (s, bs) <- sbsS, box <- bs]


-- * Box corners operation 
  {-
extremeCorners :: [Box s] -> [(Double, Double)]
extremeCorners boxes = let
    cs =  [(l+ol, h+oh) | box <- boxes
                        , let Dimension l _ h = boxDim box
                        , let Dimension ol _ oh = boxOffset box
           ]
    -- sort corner by
    cs'  = reverse cs
    in foldr (addCorner) [(0,0)] cs'


-- | Intersect a corner to list of corner
-- The corner is "hidden" if it's smaller than the existing one
addCorner :: Corner -> [Corner] -> [Corner]
addCorner c cs = concatMap (splitCorner c) cs

-- | equivalent to the intersection of 2 rectangles with
-- a top right corner at the infinite
splitCorner :: Corner -> Corner -> [Corner]
splitCorner (cx,cy) (cx',cy')
    | cx <= cx' || cy <= cy' = [(cx', cy')] -- corner shadowed
    | otherwise = [ (cx', cy), (cx,cy')]
-}


-- * Misc 

-- | Shelve or box name can have a tag, which is
-- a prefix starting with :
-- example T-shirt#shirt
extractTag :: Text -> (Text, Maybe Text)
extractTag name = let (prefix, suffix) = break (=='#') name
             in case suffix of
                  (uncons -> Just ('#', tag)) -> (prefix, Just tag)
                  _ -> (prefix, Nothing)

extractTags :: Text -> (Text, [Text])
extractTags name = (style, maybe [] (splitOn "#") tagM) where
  (style, tagM) = extractTag name


withBoxOrientations :: [OrientationStrategy] -> WH a s -> WH a s
withBoxOrientations [] action =  action
withBoxOrientations strategies action =  do
  oldStrategy <- gets boxOrientations
  let newStrategy _ _ = strategies
  modify (\wh -> wh { boxOrientations = newStrategy })
  result <- action
  modify (\wh -> wh { boxOrientations = oldStrategy })
  return result

-- * Selectors 
-- ** Applying 
-- | The phantom type guarantie that we are selecting the correct item
applyNameSelector :: NameSelector a -> (a -> Text) -> a -> Bool
applyNameSelector (NameMatches []) _ _ = True
applyNameSelector (NameMatches pats) name o = any (flip applyPattern (name o)) pats
applyNameSelector (NameDoesNotMatch pats) name o = not $ any (flip applyPattern (name o)) pats


applyTagSelector :: TagSelector s -> Tags -> Bool
applyTagSelector (TagHasKey pat) tags = case pat of
  MatchFull key -> key `member` tags
  MatchAnything -> True
  MatchGlob glob -> not (null ks) where ks = filter (Glob.match glob . unpack) (keys tags)
applyTagSelector (TagHasNotKey pat) tags = not $ applyTagSelector (TagHasKey pat) tags
applyTagSelector (TagIsKey pat) tags = case pat of
  MatchFull key -> lookup key tags == Just mempty
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [__one] -> True
    _ -> False
applyTagSelector (TagIsKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndAll valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndAll valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndSome valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndSome valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasValues valuePat) tags = let
  tagValues = mconcat (Map.elems tags)
  in matchesAllAndSome valuePat tagValues
applyTagSelector (TagHasNotValues valuePat) tags = not $ applyTagSelector (TagHasValues valuePat) tags
applyTagSelector (TagHasKeyAndNotValues key valuePat) tags = not (applyTagSelector (TagHasKeyAndValues key valuePat) tags)

applyTagSelectors :: Show s => [TagSelector s] -> (s -> Tags) -> s -> Bool
applyTagSelectors [] _ _ = True
applyTagSelectors selectors tags o =  all (flip applyTagSelector (tags o)) selectors

-- | Check all pattern are matched and matches all values
matchesAllAndAll :: [MatchPattern] -> Set Text -> Bool
matchesAllAndAll pats vals = case unmatched pats vals of
  ([], []) -> True
  _ -> False 
-- | Check all pattern matches a value (but not all values have to be matches)
matchesAllAndSome :: [MatchPattern] -> Set Text -> Bool
matchesAllAndSome pats val = case unmatched pats val of
  ([], _) -> True
  _ -> False

unmatched :: [MatchPattern] -> Set Text -> ([MatchPattern], [Text])
unmatched pats0 valSet = go [] pats0 (Set.toList valSet) where
  go unused pats [] = (pats <> unused , [])
  go unused [] vals = (unused, vals)
  go unused (pat:pats) vals = case List.partition (applyPattern pat) vals of
    ([], _) -> go (pat:unused) pats vals
    -- \^ doesn't match anything, add to unused
    (_, vals') -> go unused pats vals'

-- ** Parsing 
-- | split on |
parseSelector :: Text -> Selector a
parseSelector s = case splitOn "#" s of
  [] -> Selector(NameMatches []) []
  (name:tags) -> Selector (parseNameSelector name) (mapMaybe parseTagSelector tags)

parseNameSelector :: Text -> NameSelector a
parseNameSelector selector = let
  (constr, pat) = case uncons selector of
       Just ('!', sel) -> (,) NameDoesNotMatch sel
       _ ->  (,) NameMatches selector
  in constr $ map parseMatchPattern (splitOn "|" pat)

parseTagSelector :: Text -> Maybe (TagSelector s)
parseTagSelector tag | null tag =  Nothing
parseTagSelector tag = Just $ case break ('='  ==) tag of
  (key, "")  -> case uncons key of
                Just ('-', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                Just ('!', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                _ -> TagIsKey $ parseMatchPattern key
  ("", stripPrefix "=-" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=" -> Just values) -> TagHasValues  (mkValues values)
  -- ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  (key, stripPrefix "=+" -> Just values) -> TagHasKeyAndValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=-" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=!" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=" -> Just values) -> TagIsKeyAndValues (parseMatchPattern key) (mkValues values)
  _ -> error "Bug. Result of break = should start with = or being captured earlier"
  where mkValues = map parseMatchPattern . fromList . splitOn ";"
  
parseMatchPattern :: Text -> MatchPattern
parseMatchPattern "" = MatchAnything
parseMatchPattern pat | isGlob pat= MatchGlob (Glob.compile $ unpack pat)
parseMatchPattern pat = MatchFull pat
  

parseBoxSelector :: Text -> BoxSelector s
parseBoxSelector selector = let
  (box'location, drop 1 ->numbers) = break (=='^') selector
  (box, drop 1 -> location) = break (=='/') box'location
  in BoxSelector (parseSelector box)
              (parseSelector location)
              (parseBoxNumberSelector numbers)

parseShelfSelector :: Text -> ShelfSelector s
parseShelfSelector selector = let
  BoxSelector boxSel shelfSel _ = parseBoxSelector selector
  in ShelfSelector boxSel shelfSel

  
applyPattern :: MatchPattern -> Text -> Bool
applyPattern pat value = case pat of
  MatchAnything -> True
  MatchFull value0 -> value == value0
  MatchGlob glob -> Glob.match glob (unpack value)

-- * Position Specications
-- | read a position specification, ie an orientation and function to compute
-- the final offset depending on the box location.
-- This is to allow position specified in multiple of the box dimension
-- as well as absolute offset>
-- 
-- Syntax [=|^..][[l]:[w]:[h]][+[x]+[y]+z]
--  Example
--  |1:1:2    if the box tilted to the right, 2nd row up
--  |0+0+50   absolute offset
--  
--  offset can be combined as in
--  |1::++5
parsePositionSpec :: Text -> Maybe (Orientation, Dimension -> Dimension)
parsePositionSpec spec =  do -- Maybe
  (orientationC, offsets) <-  uncons spec
  orientation <- readOrientationMaybe orientationC
  case splitOn ("+") offsets of
    [] -> Nothing
    [""] -> Nothing
    (pos:_) | Just (' ', _) <- T.uncons pos -> Nothing
    (pos:abs) -> let 
      mul :: Double -> Maybe Int -> Double
      mul d m = case m of 
                     Nothing -> 0
                     Just n -> d * fromIntegral (n -1)
      compute :: Double -> Maybe Int -> Int -> Double
      compute dim pos offset = mul dim pos + fromIntegral offset
      (nl: nw: nh:_) = map readMay (splitOn ":" pos) ++ repeat Nothing
      (x: y: z: _) = map (fromMaybe 0) (map readMay abs ++ repeat Nothing)
      toPos dim0 = let Dimension l w h = rotate orientation dim0
                   in Dimension (compute l nl x)
                                (compute w nw y)
                                (compute h nh z)
      in Just (orientation, toPos)

readOrientations :: [Orientation] -> Text -> [Orientation]
readOrientations def os = case uncons os of
    Nothing -> []
    Just ('*', _) -> allOrientations -- all
    Just ('%', os') -> def `List.union` readOrientations def os'  -- def
    Just (o, os') -> [readOrientation o] `List.union` readOrientations def os'

-- | Orientation rules follow this syntax
-- [!] [min : ][max] [or1] [or2] ...
-- example:
-- 9 -- max 9
-- 1:9 -- min 1
-- ! don't use diagonal mode
parseOrientationRule:: [Orientation] -> Text -> [OrientationStrategy]
parseOrientationRule defOrs cs0 = let
  (diag,limitsOrs) = case uncons cs0 of
                Just ('!', s) -> (False, s)
                _ -> (True, cs0)
  (limits, orsS) = span (\c -> c `elem`( "0123456789:x" :: String)) limitsOrs
  (l, cs, h) = case splitOn "x" limits of
            [w] -> ("", w, "")
            [w,h] -> ("", w, h)
            (l:w:h:_) -> (l, w , h)
            [] -> ("","","")

  (ns, cs') = span (isDigit) cs
  n0 = fromMaybe 1 $ readMay ns
  nM = case uncons cs' of
                  Just (':', s) -> Just ( readMay s :: Maybe Int)
                  _ -> Nothing
  -- if only one number, use it as the maximum
  (min_, max_) = case nM of
    Nothing -> (0, n0)
    Just Nothing -> (n0, n0)
    Just (Just n) -> (n0, n)
  minHM = readMay h
  minLM = readMay l

  ors = case orsS of
    "" -> defOrs
    s -> readOrientations defOrs s
  in [(OrientationStrategy o  min_  max_ minLM minHM (rotateO o `elem` ors && diag)) | o <- ors ]

extractModes :: Text -> (Text, (ExitMode, PartitionMode, AddOldBoxes, Maybe SortBoxes))
extractModes modeLoc = case P.parse ((,) <$> modesParser <*> P.getInput) (unpack modeLoc) modeLoc of
  Left _ -> error "The unexpected happend. Contact your administrator" -- parser should succeed
  Right (r,e) -> (e,r)
modesParser :: P.Parser (ExitMode, PartitionMode, AddOldBoxes, Maybe SortBoxes)
modesParser = do
  exitMaybe <- P.optionMaybe (const ExitOnTop <$> P.char '^')
  es <- P.many (fmap Left partitionP <|> fmap Right boxesP)
  let (parts, boxes) = partitionEithers es
      (oldBoxes, sortBoxes) = fromMaybe boxesDefault (headMay boxes)
      partitionMode = case parts of
                      [] -> partitionDefault
                      [p] -> p
                      p:ps -> foldr POr p ps
  return ( fromMaybe exitDefault exitMaybe
         , partitionMode
         , oldBoxes
         , sortBoxes
         )


  where partitionP = asum $ map go
                          [ (P.char '~', PAboveOnly)
                          , (P.char ':', PRightOnly)
                          , (P.char '%', PBestEffort)
                          , (P.char '-', POverlap)
                          , (P.char '_', PBehind)
                          ]
                          <> map go
                          [ (P.string "right", PRightOnly)
                          , (P.string "above", PAboveOnly)
                          , (P.string "best", PBestEffort)
                          , (P.string "overlap", POverlap)
                          , (P.string "behind", PBehind)
                          ]
        go (p, v) = const v <$> P.try p
        boxesP = asum $ map go
                [ (P.char '@', (AddOldBoxes, Just SortBoxes))
                , (P.char '+', (AddOldBoxes, Just DontSortBoxes))
                ]
        exitDefault = ExitLeft
        boxesDefault = (NewBoxesOnly, Nothing)
        partitionDefault = PRightOnly
          
                 

          
    
  
-- * Warehouse Cache 
-- ** Property stats 
-- | Retrieve property stats (and compute if needed)
propertyStatsFor :: OrderingKey -> WH PropertyStats s
propertyStatsFor prop = do
  cacheRef <- whCache
  cache <- lift $ readSTRef cacheRef
  case lookup (tshow prop) (propertyStats cache) of
    Just stat -> return stat
    Nothing -> computePropertyStats prop

-- | Computes or refresh the statistics for the given property
computePropertyStats :: OrderingKey -> WH PropertyStats s
computePropertyStats prop = do
  -- scann all object and make a map of the different values
  wh <- get
  boxList <- mapM findBox $ boxes wh
  values <- mapM do \box -> expandOrdkey box Nothing prop
                 do boxList
  let stats = mkPropertyStats $ concat values
  -- update cache
  cacheRef <- whCache
  lift $ modifySTRef cacheRef  (\cache -> cache {propertyStats = Map.insert (tshow prop) stats (propertyStats cache) })
  return stats
  
mkPropertyStats :: [Text] -> PropertyStats
mkPropertyStats values = PropertyStats{..} where
  totalCount = length valueRank
  value'count = Map.fromListWith (+) $ map (,1) values
  valueRank = Map.fromList $ zip (map fst $ sortOn (Down . snd )
                                            (Map.toList value'count)
                                 )
                                 [1..]
  valueIndex = Map.fromList $ zip (sort $ keys valueRank) [1..]
  
clearCache :: WH () s
clearCache = do
  cachem <- gets whCacheM
  case cachem of
    Nothing -> return ()
    Just cache -> lift $ writeSTRef cache (emptyOperationCache)
  
-- | Create an empty cache if necessary
whCache :: WH (STRef s (OperationCache s)) s
whCache = do
  cachem <- gets whCacheM
  case cachem of
    Just cache -> return cache
    Nothing -> do
      cacheRef <- lift $ newSTRef emptyOperationCache 
      wh <- get
      put wh {whCacheM = Just cacheRef}
      return $ cacheRef
      
  
getOrCreateBoxTagMap :: Text -> WH (Map Text [Box s])  s
getOrCreateBoxTagMap prop = do
  cacheRef <- whCache
  cache <- lift $ readSTRef cacheRef 
  case lookup prop (boxTagMapMap cache) of
    Just boxMap -> {-traceShow ("Reuse cache for prop: " <> prop) $ -} return boxMap
    Nothing -> do
      boxMap <- __getBoxTagMap prop
      lift $ modifySTRef cacheRef (\cache' -> cache' {boxTagMapMap = Map.insert prop boxMap (boxTagMapMap cache')})
      return boxMap

__getBoxTagMap :: Text -> WH (Map Text [Box s]) s
__getBoxTagMap prop = do
  boxIds <- gets boxes
  boxes <- mapM findBox boxIds
  return $ Map.fromListWith (<>) [ (value, [box])
                                 | box <- toList boxes
                                 , value <- getTagValues box prop
                                 ]

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
  
