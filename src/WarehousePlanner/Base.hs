{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module WarehousePlanner.Base
( assignShelf
, boxCoordinate
, boxContentPriority
, boxGlobalPriority
, boxStylePriority
, boxPosition
, boxPositionSpec
, boxRank
, boxShortContent
, boxStyleAndContent
, buildWarehouse
, clearCache
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
, findBoxByNameSelector
, findShelfBySelector
, findShelfBySelectors
, findBoxByNameAndShelfNames
, findBoxByNameAndShelfNamesWithPriority
, findBoxByShelf
, getOrCreateBoxTagMap
, findShelvesByBoxNameAndNames
, incomingShelf
, limitByNumber, UseDefault(..)
, maxUsedOffset
, minUsedOffset
, modifyTags
, multipleContentToSingles
, module WarehousePlanner.Type
, negateTagOperations
, newBox, newBox'
, newShelf
, orTrue
, partitionModeParser
, parseTagOperation
, parseTagOperations
, parseTagAndPatterns
, parsePositionSpec
, parseOrientationRule
, partitionBoxes
, partitionShelves
, printTagOperation, printTagOperations
, printDim
, readOrientations
, readTagAndPatterns
, replaceSlashes
, shelfBoxes
, updateBox
, updateBoxTags
, updateBoxTags'
, updateShelf
, updateShelfTags
, usedDepth
, sortOnIf
, withBoxOrientations
, withBoxOrientations'
, newWHEvent
, newBaseEvent
, Priority, Keys
, firstM
)
where
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import Text.Printf(printf)
-- import Data.Map.Strict qualified as Map'
import Data.Map.Lazy qualified as Map
import Control.Monad.State(gets, get, put, modify)
import Data.Map.Merge.Lazy(merge, preserveMissing, mapMaybeMissing, zipWithMaybeMatched)
-- import Data.List(sort, sortBy, groupBy, nub, (\\), union, maximumBy, delete, stripPrefix, partition)
import Data.List qualified as List
import Data.Foldable qualified as Foldable
import Data.STRef
import Data.Sequence ((|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import WarehousePlanner.Type
import WarehousePlanner.Selector
import WarehousePlanner.SimilarBy
import WarehousePlanner.History
import Diagrams.Prelude(white, black, darkorange, royalblue, steelblue)
import Data.Text (uncons, stripPrefix)
import Data.Text qualified as T 
import Data.Char (isLetter, isDigit, isAlphaNum)
import Data.Time (diffDays)
import Data.Semigroup (Arg(..))

import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import GHC.Prim 
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import System.FilePath.Glob qualified as Glob
import Data.Bitraversable (Bitraversable(..))
import Data.Maybe (fromJust)
-- import GHC.Exts (groupWith)


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

-- | Computes the max length or height used within a shelf
-- | by all the boxes. Should be able to be cached.
-- maxUsedOffset :: (Dimension -> Double) -> Shelf -> WH
maxUsedOffset ::  Shelf s -> WH Dimension s
maxUsedOffset shelf = do
    boxes <- findBoxByShelf shelf
    return $ maxDimension $ map boxCorner boxes

minUsedOffset ::  Shelf s -> WH Dimension s
minUsedOffset shelf = do
    boxes <- findBoxByShelf shelf
    return $ minDimension $ map boxOffset boxes
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


findBoxByNameSelector :: (NameSelector Box) -> WH [Box s] s
findBoxByNameSelector selector = do
  boxIdMap <- gets boxMap
  let matcher = applyNameSelector (coerce selector) runIdentity
                --                                  ^^^^^^^^^^^
                --                                      |
                --  +-----------------------------------+
                --  |
                --  Trick so that in applyNameSelector type signature
                --  a = Identity , a s = Identity Text
  boxess <- forM (Map.toList boxIdMap)
                \(style, ids) ->
                  if matcher $ Identity style
                  then
                    mapM findBox ids
                  else return mempty
  return $ toList $  mconcat boxess

findShelfByBox :: Box' box => box s -> WH (Maybe (ShelfId s)) s
findShelfByBox box' = do
  box <- findBox box'
  return $ boxShelf box

-- | find shelf by name and tag
findShelfBySelector :: Selector Shelf -> WH [ShelfId s] s
findShelfBySelector selector = map shelfId <$> findShelfBySelector' selector
findShelfBySelector' :: Selector Shelf -> WH [Shelf s] s 
findShelfBySelector' (Selector nameSel tagSels ) = do
  shelfIds <- toList <$> gets shelves
  shelves0 <-  filterByNameSelector (mapM findShelf shelfIds) shelfName  nameSel
  let shelves1 = filter (applyTagSelectors tagSels shelfTag) shelves0
  return shelves1
-- | Find shelf by name and tag
-- but respect alternative order
-- ie B|A will return B before A
-- B*|A* will return Bs (sorted alphabetically) the A* (alphabetically)
findShelfBySelectors :: [Selector Shelf] -> WH [ShelfId s] s
findShelfBySelectors selectors = do
  shelvess <- mapM findShelfBySelector' selectors
  return [ shelfId shelf
         | shelves <- shelvess
         , shelf <- sortOn shelfName shelves
         ]



filterShelfByTag :: [TagSelector Shelf] -> Shelf s -> Bool
filterShelfByTag selectors shelf = applyTagSelectors selectors shelfTag shelf

filterBoxByTag :: [TagSelector Box]-> Box s -> Bool
filterBoxByTag selectors box =  applyTagSelectors selectors boxTags box

-- | Compiles a match against a glob pattern if Necessary

filterByNameSelector :: WH [a s] s -> (a s -> Text) -> (NameSelector a) -> WH [a s] s
filterByNameSelector objects objectName selector = do
   let matcher= applyNameSelector selector objectName
   filter matcher <$> objects


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
findBoxByNameAndShelfNames :: BoxSelector -> WH [Box s] s
findBoxByNameAndShelfNames selector = map fst <$> findBoxByNameAndShelfNamesWithPriority selector
findBoxByNameAndShelfNamesWithPriority :: BoxSelector -> WH [(Box s, Priority)] s
findBoxByNameAndShelfNamesWithPriority ( BoxSelector ( Selector (NameMatches [])
                                                    [ TagIsKeyAndValues (MatchFull prop)
                                                                        [VMatch (MatchFull value)]
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
                    Just shelf'  -> if applySelector shelfSel shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'shelfS =  [ (box, shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map (first fst) $ limitByNumber UseDefault numSel $ map (,Nothing) box'shelfS


findBoxByNameAndShelfNamesWithPriority (BoxSelector boxSel shelfSel numSel) = do
  -- all boxes matching name
  allBoxesBeforeTag <- findBoxByNameSelector (nameSelector boxSel)
  let allBoxes = filter (applyTagSelectors (tagSelectors boxSel) boxTags) allBoxesBeforeTag
  box'shelfms <- forM allBoxes $ \box -> do
      shelfIdM <- findShelfByBox (boxId box)
      shelfM <- traverse findShelf shelfIdM
      let shelf = case shelfM of
                    Just shelf'  -> if applySelector shelfSel shelf'
                                    then shelfM
                                    else Nothing
                    _ -> Nothing

      return $ (box, shelf)


  -- we need the shelf name to sort box by shelves
  let box'nameS =  [ (box, shelf) | (box, Just shelf) <- box'shelfms] 
        
  -- filter boxes by number
  return . map (first fst) $ limitByNumber UseDefault numSel $ map (,Nothing) box'nameS


-- | Limit a box selections by numbers
limitByNumber :: UseDefault -> BoxNumberSelector -> [((Box s, Shelf s), Maybe Priority)] -> [((Box s, Shelf s), Priority)]

limitByNumber useDefault selector@BoxNumberSelector{..} unsortedBoxes = let
  grouped = groupBySelector useDefault selector unsortedBoxes
  in take_ nsTotal $ concatMap (take_ nsPerShelf . concat . map (take_ nsPerContent)) grouped
  where take_ :: Limit -> [a] -> [a]
        take_ NoLimit = id
        take_ sel = maybe id (drop . (subtract 1)) (liStart sel) . maybe id take (liEnd sel)

   -- where globalKey = snd


groupBySelector :: UseDefault -> BoxNumberSelector -> [((Box s, Shelf s), Maybe Priority)] -> [[[((Box s, Shelf s), Priority)]]]
groupBySelector useDefault selector unsortedBoxes = let
   bs'p = [ (b's, boxFinalPriority useDefault selector b's'p)
          | b's'p@(b's,_) <- unsortedBoxes
          ]
   byContent = groupSimilar (\(_, (g, (s,_) )) -> (g,s)) $ sortOn snd bs'p
   byStyle = groupSimilar (\(SimilarBy g's _ _) -> fst g's) byContent
   in map ( map unSimilar
          . unSimilar
          )
          byStyle

   

type Keys = [Either (Down (Either Int Text)) (Either Int Text)]
type Priority = (Keys, (Keys , Keys))
keyFromLimitM :: Limit -> Keys -> Box s -> Shelf s ->  Keys
keyFromLimitM limit def box shelf =
  case liOrderingKey limit of
    [] -> def
    keys -> map evalKey keys
  where evalKey (k, order) = let
          val = case k of
                     OrdTag tag0 -> let (tag, evaluator) = parseEvaluator tag0
                                    in case evaluator $ getTagValuesWithPresence box tag of
                                            Nothing -> Right maxString
                                            Just v -> maybe (Right v) Left (readMay v) -- :: Either Int Text
                     OrdAttribute att -> expandIntrinsic att box shelf
          in case order of
               NormalOrder -> Right val
               ReverseOrder -> Left $ Down val

        maxString = T.replicate 8 (singleton '~')


firstM :: (Applicative f, Bitraversable t) => (a -> f a') -> t a b -> f (t a' b)
firstM f = bitraverse f pure
-- limitBy :: Ord k => (Box s -> k) -> Int -> [Box s] -> [a]
  
data UseDefault = UseDefault | DontUseDefault
boxFinalPriority :: UseDefault -> BoxNumberSelector -> ((Box s, Shelf s), Maybe Priority) -> Priority
boxFinalPriority useDefault BoxNumberSelector{..} ((box, shelf), pm) = let -- reader
  (pglob, (pstyle, pcon)) = fromMaybe ([], ([], [])) pm 
  global = with nsTotal boxGlobalPriority  [Right $ boxStyle box] pglob
  style = with nsPerShelf boxStylePriority [Right $ boxContent box] pstyle
  content = with nsPerContent boxContentPriority [Left break , Left $ bId ] pcon
  -- boxes with break specification first
  break = case getTagValuem box "@start" of
            Nothing -> 1
            _ -> 0 
  --                                                    ^^^^
  --                                                    by default
  -- use the boxid so that the old box is selected first
  BoxId_ (Arg bId _) = _boxId box
  in ( global 
     , (style
       , content
       )
     )
  where useBase = maybe True liUseBase
        with sel p base pb = case useDefault of
                            UseDefault ->  keyFromLimitM sel [Right $ Left $ p box] box shelf <> if useBase (Just sel) then (fmap Right base) else pb
                            DontUseDefault ->  keyFromLimitM sel pb box shelf 


  
-- | Use similar syntax to boxes but returns shelves instead
findShelvesByBoxNameAndNames :: ShelfSelector -> WH [Shelf s] s
findShelvesByBoxNameAndNames (ShelfSelector SelectAnything shelfSel) = findShelfBySelector shelfSel >>= mapM findShelf
findShelvesByBoxNameAndNames (ShelfSelector (Selector (NameDoesNotMatch [MatchAnything]) []) shelfSel)= do
    -- only get empty shelves
    shelves <- findShelfBySelector shelfSel >>= mapM findShelf
    let hasBoxes = null .  _shelfBoxes 
    return $ filter hasBoxes shelves
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



partitionBoxes :: (Traversable f, Box' boxId) => BoxSelector -> f (boxId s, Priority) -> WH ([(Box s, Priority)], [(Box s, Priority)]) s
partitionBoxes BoxSelector{..} bid'ps = do
  let (bids, priorities) = List.unzip $ Foldable.toList bid'ps
  box'shelves <-  flip zip priorities <$> findBoxesWithShelf bids
  let (goods, bads) = partition isSelected box'shelves
      sorted = limitByNumber DontUseDefault numberSelector $ map (fmap Just) goods
      getBoxId = boxId . fst . fst
      truncated = filter (flip (notMember . getBoxId) (Set.fromList $ fmap getBoxId sorted)) goods
  return (map (first fst) sorted, map (first fst) $ truncated <> bads)
  where isSelected ((box, shelf),_p) = applySelector boxSelectors  box && applySelector shelfSelectors shelf
   
  

-- | Only returns boxes which have a shelf, which should be all, in theory
findBoxesWithShelf :: (Traversable f, Box' boxId) => f (boxId s) -> WH (f (Box s, Shelf s)) s
findBoxesWithShelf = mapM go where
   go bId = do 
       box <- findBox bId
       shelfM <- forM (boxShelf box) findShelf
       case shelfM of
         Nothing -> error $ "No shelf for box" <> show box
         Just shelf -> return (box, shelf)

partitionShelves :: (Traversable f, Shelf' shelfId) => ShelfSelector -> f (shelfId s) -> WH ([Shelf s], [Shelf s]) s
partitionShelves ShelfSelector{..} sIds = do
  shelf'boxess <- mapM findShelfWithBoxes sIds
  let (goods, bads) = partition isSelected $ Foldable.toList shelf'boxess
  return $ (map fst goods, map fst bads)
  where isSelected (shelf, boxes) = applySelector sShelfSelectors shelf
                                  && case sBoxSelectors of
                                       SelectAnything -> True
                                       bsel -> any (applySelector bsel) boxes
                                     
findShelfWithBoxes :: Shelf' shelfId => shelfId s -> WH (Shelf s, [Box s]) s
findShelfWithBoxes shelfId = do
   shelf <- findShelf shelfId
   boxes <- findBoxByShelf shelf
   return (shelf, boxes)
    
  

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
emptyWarehouse today = Warehouse mempty mempty (error "No layout defined")
                                 (const defaultBoxStyling)
                                 (const defaultShelfStyling)
                                 defaultBoxOrientations Nothing today
                                 0
                                 NoHistory []

newShelf :: Text -> Maybe Text -> Dimension -> Dimension -> Double -> BoxOrientator -> FillingStrategy -> WH (Shelf s) s
newShelf name tagm minD maxD bottom boxOrientator fillStrat = mdo
        let -- tags = case splitOn "#" <$> tagm of
            --   Nothing -> mempty
            --   Just [""] -> mempty
            --   Just tags' -> fromMaybe mempty $ modifyTags (map parseTagOperation tags') mempty
            tags = fromMaybe mempty $ fmap parseTagOperations tagm >>= (flip modifyTags mempty)
        uniqueRef@(Arg _i ref) <- newUniqueSTRef shelf
        let shelf = Shelf (ShelfId_ uniqueRef) mempty name tags minD maxD LeftToRight boxOrientator fillStrat bottom
        writeCurrentRef ref shelf

        modify \warehouse ->  warehouse { shelves = shelves warehouse |> ShelfId_ uniqueRef }
        updateShelfTags [] shelf

newBox :: Shelf' shelf => Text -> Text ->  Dimension -> Orientation -> shelf s  -> [Orientation]-> [Text] -> WH (Box s) s
newBox = newBox' Nothing
newBox' :: forall s shelf . Shelf' shelf => Maybe (Box s) -> Text -> Text ->  Dimension -> Orientation -> shelf s  -> [Orientation]-> [Text] -> WH (Box s) s
newBox' boxM style content dim or_ shelf ors tagTexts = mdo
    let tags' = map (parseTagOperation . omap replaceSlash) tagTexts
        dtags = dimensionTagOps dim
        -- create "'content" tag
        contentTag = (omap replaceSlash $ cons '\'' content, SetTag)
        tags = fromMaybe mempty $ modifyTags (contentTag : makeContentTags content <> tags' <> dtags) mempty
                                  --   ^ apply dimension tags after tags so dimension override tags

    uniqueRef <- case boxM of 
                  Nothing -> newUniqueSTRef box
                  Just box | (BoxId_ ref) <- _boxId box -> return ref
    let (Arg _ ref) = uniqueRef
    let box = Box (BoxId_ uniqueRef) (Just $ shelfId shelf) style content dim mempty or_ ors tags (extractPriorities tags defaultPriorities) (extractBoxBreak tags)
    -- don't update box if not needed, to not polute history and make diff possible
    moveShelf <- case boxM of
          Just box | boxShelf box == Just (shelfId shelf) -> return False
          Just box | Just bshelfId <- boxShelf box -> do 
              bshelf <- findShelf bshelfId
              unlinkBox (boxId box) bshelf
              return True
          _ -> return True
    when moveShelf do
         shelf' <- findShelf shelf
         linkBox (BoxId_ uniqueRef) shelf'
    writeCurrentRef ref box
    case boxM of
        Nothing -> do
             -- modify \warehouse ->  warehouse { boxes = boxes warehouse |> BoxId_ uniqueRef }
             modify \warehouse ->  warehouse { boxMap = snd $ Map.insertLookupWithKey (\_ new old -> old <> new)
                                                                               style (Seq.singleton $ BoxId_ uniqueRef)
                                                                               (boxMap warehouse)
                                             }
        Just _ -> return ()
    return box
    
-- |  create #content1=A, #content2=B from A&B
makeContentTags :: Text -> [Tag'Operation]
makeContentTags content = 
  case splitOnNonEscaped "&" content of
   [_] -> []
   contents -> ("mixed", SetTag) : [ ("content" <> tshow i, SetValues [c])
                                   | (c, i) <- zip contents [1..] 
                                   ]
                                   
-- | replace box with multiple content to multiple box with unmixd content
-- exa A&b ->  [A, B]
multipleContentToSingles :: Box s -> NonEmpty (Box s)
multipleContentToSingles box =
  case splitOnNonEscaped "&" (boxContent box) of
     contents@(_:_) -> fromJust
                       $ nonEmpty [ box { boxContent = content
                                        , boxTags =  (Map.singleton ("'" <> content) mempty) 
                                                     <> Map.filterWithKey  toKeep (boxTags box)
                                        }
                                  | content <- contents
                                  ]
     _ -> box :| []
  where toKeep t _ = not ("content" `isPrefixOf` t || t == ("'" <> boxContent box))

newUniqueSTRef :: a s -> WH (Arg Int (HiSTRef a s)) s
newUniqueSTRef object = do
  ref <- lift $ newHiSTRef NoHistory object
  unique0 <- gets whUnique
  let unique = unique0 + 1
  modify (\w -> w { whUnique = unique })
  return $ Arg unique ref

writeCurrentRef :: HiSTRef a s -> a s -> WH () s
writeCurrentRef ref a = do
   current <- gets whCurrentEvent
   lift $ writeHiSTRef current ref a
   
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
  where split = splitOnNonEscaped ";"
 
parseTagOperations :: Text -> [Tag'Operation]
parseTagOperations tag =
 case splitOnNonEscaped "#" tag of
   [] -> []
   [""] -> []
   tags -> map parseTagOperation tags
  
  
printTagOperation :: Tag'Operation -> Text
printTagOperation (tag, op) = case op of
  SetTag           -> tag
  RemoveTag        -> "-" <> tag
  SetValues []     -> "-" <> tag <> "="
  SetValues vals   -> tag <> "=" <> intercalate ";" vals
  AddValue val     -> tag <> "=+" <> val
  RemoveValue val
    | "-" `isPrefixOf` tag -> tag <> "=" <> val
    | otherwise            -> tag <> "=-" <> val
  
printTagOperations :: [Tag'Operation] -> Text
printTagOperations = intercalate "#" . map printTagOperation

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


-- | Parse tags operations from a list a text.
-- If @\@include@ or @\@exclude@ is used, the tags on the right
-- will be used as glob pattern to filter the local tags
-- This allows to read boxes but only set a s
{-  rST::tag-and-patterns
 
Some sections accepts a list of tags and patterns as parameters. Those
tags are usually applied on "success" and applied as well as the tags of
a specific line. Tag and patterns allows to filter the final tags using
thes special tags ``@exclude`` and ``@include``.

``@exclude`` will exclude all the tags matching the (glob) patterns at
is right

``@include`` will only include all tags matching the (glob) patterns at
is right.

This is particularly usefull when import an existing file when some
selected needs to be imported. Example, given the stock.org file

::

  :STOCKTAKE:
  Bay No,Quantity,Style,Length,Width,Height,Orientations
  shelf,1,A#status=active#bg=black,10,20,20,
  :END:

This will create a box A with the two properties ``status`` and ``bg``.
Replace ``:STOCKTAKE:``, with ``:STOCKTAKE_@include#stat*``, will only
the ``status`` property (it is the only one matching the pattern
``stat*``. This can be achieved without modifying the file but importing
it with extra tag

::

  :IMPORT:
  file/stock.org#@include#stat*
  :END:
::rST -}
parseTagAndPatterns :: [Text] ->  [Text] -> [Tag'Operation]
parseTagAndPatterns tagsAndPatterns localTags = 
  let (defaultTags, pats) = break (`elem` ["@exclude", "@include"]) tagsAndPatterns
      globs = map (Glob.compile . unpack) $ drop 1 pats
      keepTagOp = case pats of
        "@exclude":_ -> \(tag, _) -> not $ any (flip Glob.match (unpack tag)) globs
        "@include":_ -> \(tag, _) -> any (flip Glob.match (unpack tag)) globs
        _ -> const True
  in map parseTagOperation defaultTags <> filter keepTagOp (map parseTagOperation localTags)
  
readTagAndPatterns :: [Text] -> [Text] -> [Text]
readTagAndPatterns tagsAndPatterns localTags = maybe [] flattenTags $ modifyTags (parseTagAndPatterns tagsAndPatterns localTags) mempty 
  

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
                updateBox (\b -> b { boxShelf = Nothing}) box
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



updateBox :: (Box' box) =>  (Box s ->  Box s) -> box s-> WH (Box s) s
updateBox f box0 = do
    box <- findBox box0
    let box' = f box
    writeCurrentRef (getRef box') box'
    return box'

updateShelf :: (Shelf' shelf) => (Shelf s -> Shelf s ) -> shelf s -> WH (Shelf s) s
updateShelf f s =  do
    shelf <- findShelf s
    let shelf' = f shelf
    writeCurrentRef (getRef shelf') shelf'
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
applyTagOperation (RemoveValue value) olds = let
    set = Set.delete value olds
    in if null set
       then Nothing
       else Just set

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
boxCoordinate = boxCoordinate' floor

boxCoordinate' :: (Double -> Int) -> Box s -> Dimension
boxCoordinate' rounding = fst . boxPosition' rounding
                     
-- | Box coordinate + left over                   
boxPosition ::  Box s -> (Dimension, Dimension)
boxPosition = boxPosition' floor
boxPosition' :: (Double -> Int) -> Box s -> (Dimension, Dimension)
boxPosition' rounding box  = let
  (Dimension ol ow oh) = boxOffset box
  (Dimension l w h) = boxDim box
  go o d = let q = fromIntegral $ rounding (o / d)
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
expandAttributeMaybe :: forall s . Text -> Maybe (Box s -> Int -> WH Text s)
expandAttributeMaybe text = let
  wrap :: Box s -> Int -> (Text -> Box s -> Int -> WH Text s) -> Text -> WH Text s
  wrap box index f subtext =
    case (uncons subtext, T.breakOn "}" subtext) of
      -- (_, "") -> f subtext box
      (Just ('{', _) , (key, leftOver)) -> (<> drop 1 leftOver) <$> f key box index
      _ -> f subtext box index
      
  in case splitOnNonEscaped "$" text of
     prefix:segments -> Just $ \box i -> do
         -- expand (vs a simple mapM is necessarry to catche the $$ case)
         -- $${comma}..$... is split to "", "{comma}..", "..."
         -- "" indicate a $$ but we need to skip the expansion of {hello} 
         -- We want $${comma}  to be parsed as $$ {comma} 
         -- not as $ ${comma}
          let expand :: [Text] -> WH [Text] s
              expand [] = return []
              expand ("":x:xs) = do
                expanded <- expand xs
                return $ "$" : x : expanded
              expand (x:xs) = do
                 exp <- wrap box i expandAttribute' x
                 expanded <- expand xs
                 return $ exp : expanded
          expandeds <- expand segments  -- mapM (wrap box i expandAttribute') segments
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
{- rST::group-function
Tags be set not only on the value of a propetry but also using a function depending on the current value of a property as well as all the values of that property for other boxes.

This allows for example to calculate the rank or the index of value within the all the other boxes.

Index and rank
""""""""""""""

this tag across all the boxes. The index is the index of the value of
the values sorted alphabetically whereas the rank is the index of the
value sorted by number of occurance. For example, if we have 1 boxes
scanned by operator A, 3 boxes by operator B And 2 by operator C

::

   Box1#operator=A
   Box2#operator=B
   Box3#opertor=B
   Box4#opertor=B
   Box5#opertor=C
   Box6#opertor=C

The index of A is 1, B is 2 and C is 3, whereas the rank of A is 3 (only
1 occurrence), B is 1 (3 occurrences) and C is 2. To get the rank or the
index the special property syntax is ``$index``\ op\ ``[``\ tag\ ``]``
and ``$rank``\ op\ ``[``\ tag\ ``]`` where op is an optional operator
``-`` ``%`` or ``^`` followed by an integer.

::

   -n -- limit the index to n 
   %n -- cycle within n value (index modulo n)
   ^n -- map all indexes to the range 1-n

Example, given the previous boxes

::

   $index[operator] -- A -> 1,  B ->  2, C -> 3
   $rank[operator] -- A -> 3,  B ->  1, C -> 2
   $index-2[operator] -- A -> 1,  B ->  2, C -> 2 (limit to 2)
   $index%2[operator] -- A -> 1,  B ->  2, C -> 1 (cycle to 2)
   $index^5[operator] -- A -> 1,  B ->  3, C -> 5 

This can used to given a different colours to each boxes depending on
the operator

::

   bg=Spectral10-$index%10[operator]

Where Spectral10 is the 10 colours variant of the Spectral palette Of
course different colours can be mixed

::

   bg=Spectral10-$index%10[style];Greys4-$index^4[style]

Let's suppose we have 40 different styles. $index%10 will return 1 for
1, 11, 21 etc ... But $index^4 will return 1 for 1-10, 2, for 2-19 etc
... This way each of the 40 styles have a different colours.

ago
"""

For dates, the
``op``\ ``[``\ ``tag``\ ``]``\ ``transform the date not into a index but to an integer but the number of days ago (from today). Operators have a different meaning (subject to change)``

::

   $ago[date] -- number of days ago
   $ago-0[date] -- number of years ago
   $ago-n[date] -- normalize  date range To n
   $ago-n[date] -- normalize  date range To n
   $ago%n[date] -- year moduln n
   $ago^0[date] -- within last week (1) , month (2), 3 moths (3), 6 months (4), year (5) , 3 years (6), more (7)$ago^n[date] -- log so that the current date range go from 1 to n

transforming value
""""""""""""""""""

The index/position o the box in the current selection (taking ordering
into account) can be used with ``$n``\ op\ ``[``\ [format]\ ``]``,
``$select``\ op\ ``[``\ value1\ ``|``\ value2\ ``|``...\ ``]``,
``$cycle``\ op\ ``[``\ value1\ ``|``\ value2\ ``|``...\ ``]``. This can
be used to generate different values for each boxes

::

   --                       Box1    Box 2  Box3   Box4
   $n[]              -- 1       2      3      4
   $n*3[]            -- 3       6      9      12
   $n%3[]            -- 1       2      3      1
   $n^3[]            -- 1       2      3      3
   $n[%02d]          -- 01      02     03     04
   $select[red|BLUE] -- red     BLUE   BLUE   BLUE
   $select[red|BLUE|]-- A       BLUE      
   $cycle[red|BLUE]  -- red     BLUE   red    BLUE
   
   
Event
"""""

``event`` and `eventId` will expand to the last event which modified the given tag.
Example::

   $even[bg]
   
Expands to the name of the event which last bodify ``bg``.
::rST -}
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
             let e = case splitOnNonEscaped "|" propText of
                          [] -> tshow i
                          values -> values List.!! (min (length values) (evalArg arg i) - 1)
             return $ e <> xs
    "cycle" -> do
                 let e = case splitOnNonEscaped "|" propText of
                       [] -> tshow i
                       values -> values List.!! mod (evalArg arg i - 1) (length values)
                 return $ e <> xs
    "event" -> do
        event <- forEvent box prop
        return $ evDescription event <> xs
    "eventId" -> do
        event <- forEvent box prop
        return $ tshow event <> xs

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
          -- | Find the event for when the given prop as been modified
          forEvent box prop = do
                    h :| hs <- getBoxHistory box >>= historyFor
                    let fm =
                              case prop of
                                   OrdTag "" -> Nothing
                                   OrdAttribute "" -> Nothing
                                   OrdTag tag -> expandAttributeMaybe ("[" <> tag <> "]")
                                   OrdAttribute att -> expandAttributeMaybe ("{" <> att)
                    fst <$> case fm  of
                         Nothing ->  return h
                         Just f' -> do
                              let f box = f' box 1
                              propValue <- f box
                              let next current (previous: ps) = do
                                    new <- f (snd previous)
                                    if new /= propValue -- found
                                    then return current
                                    else next previous ps
                                  next current [] = return current
                              next h hs 
                              
                    



expandAttribute' text = \_ _ -> return $ cons '$' text
{- rST::box-attributes
Certain attributes like the current location or orientation of a box can
be used to set a new tag with the corresponding value. The following
attributes are available.

-  ``${shelfname}``:  current shelf
-  ``${shelftags}``:  tag of the current shelf
-  ``${fit}``:  How the box fits in the shelf : ``fit``, ``tight``, or ``out``
-  ``${orientation}``:  current orientation
-  ``${style}``:  current box style
-  ``${id}``:  the box id 
-  ``${content}``:  current box content (or colour)
-  ``${con}``:  short box content
-  ``${boxname}``:  box style + content
-  ``${dimension}``:  box dimension in cm
-  ``${offset}``:  box offset (within the shelf) in cm
-  ``${coordinate}``:  box coordinate (as if row and column of similar
-  ``${position-spec}``:  orientation , coordinate and relative offset if needded
   boxes) (start at 1)
-  ``${ol}``:  length coordinate (rounded)
-  ``${ow}``:  width coordinate (rounded)
-  ``${oh}``:  height coordinate (rounded)
-  ``${cl}``:  length coordinate
-  ``${cw}``:  width coordinate
-  ``${ch}``:  height coordinate
-  ``${ox}``:  length offset
-  ``${oy}``:  width offset
-  ``${oz}``:  height offset
-  ``${@}`` shorthand for ``${@global}${@style}${@content}``
-  ``${@content}``:  content priority
-  ``${@style}``:  style priority
-  ``${@global}``:  global priority
-  ``${volume}``:  volume 
-  ``${vol}``:  volume  in scientific notation
-  ``#{hash}``: the character ``#``
-  ``#{comma}``: the character ``,``
-  ``#{divide}``: the character ``/``
-  ``#{dollar}``: the character ``$``

Example

::

   /pending,loc=${shelfname}  => All boxes in the pending location will be tagged with "loc=pending".
::rST -}
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
expandIntrinsic' "cl" box _shelf =let (Dimension ol _ _ ) = boxCoordinate box in  Left $ round ol
expandIntrinsic' "cw" box _shelf =let (Dimension _ ow _ ) = boxCoordinate box in  Left $ round ow
expandIntrinsic' "ch" box _shelf =let (Dimension _ _ oh ) = boxCoordinate box in  Left $ round oh
expandIntrinsic' "ol" box _shelf =let (Dimension ol _ _ ) = boxCoordinate' round box in  Left $ round ol
expandIntrinsic' "ow" box _shelf =let (Dimension _ ow _ ) = boxCoordinate' round box in  Left $ round ow
expandIntrinsic' "oh" box _shelf =let (Dimension _ _ oh ) = boxCoordinate' round box in  Left $ round oh
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
expandIntrinsic' "position-spec" box _shelf = Right $ boxPositionSpec box
expandIntrinsic' "volume" box _shelf = Right $ pack $ printf "%09.2f" (boxVolume box)
expandIntrinsic' "vol" box _shelf = Right $ pack $ printf "%.2e" (boxVolume box)
expandIntrinsic' "con" box _shelf = Right $ boxShortContent box
expandIntrinsic' "id" box _shelf = let BoxId_ (Arg bId _)  = boxId box
                                   in Left bId
expandIntrinsic' "ox" box _shelf = Right $ pack $ printf "%.f" (dLength $ boxOffset box)
expandIntrinsic' "oy" box _shelf = Right $ pack $ printf "%.f" (dWidth $ boxOffset box)
expandIntrinsic' "oz" box _shelf = Right $ pack $ printf "%.f" (dHeight $ boxOffset box)
expandIntrinsic' prop _box _shelf = 
    case prop of
       "hash" -> Right "#" 
       "comma" -> Right ","
       "divide" -> Right "/"
       "dollar" -> Right "$"
       _ -> error . unpack $ prop <> " is not a property"  -- Right $ "${" <> prop <> "}"


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
        stat <- asum $ map P.string  ["rank", "index", "ago", "n", "select", "cycle", "eventId", "event"]
        opM <- P.optional  do
                op <- P.satisfy (`elem` ("-+*/%^" :: String))
                Just n <- P.optional P.decimal
                return (op, n)
        att <- parseTag <|> parseAttribute
        leftOver <- P.takeRest
        return $ (stat, opM, att, leftOver)
  parseTag = OrdTag <$> between '[' ']'
  parseAttribute = OrdAttribute <$> (P.char '{' >> P.takeWhileP Nothing (/= '}')) --  <* P.char '}'
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


-- | Split content on special character
-- BLK&WHT -> B&W
boxShortContent :: Box s -> Text
boxShortContent = shorten . boxContent
shorten :: Text -> Text
shorten content =  let
   grouped = groupBy (\a b -> isAlphaNum a == isAlphaNum b) content
   in case grouped of
      [one] -> one
      [one,two] -> take 2 one <> take 1 two
      many -> foldMap (take 1) many
   
  
  
-- | Syntax tag[?] 
{- rST::evaluator
Tags and properties can be evaluated in different way resulting in
different values. The syntax is the same for properties and tags

-  ``?[then][:[:else]]`` test if the value is present.
   if no parameters are given the value will the tag if preset
-  ``??default`` value of default if the value is not preset
-  ``:[start][:[end]]`` extract substring. ``start`` and ``end`` can be
   a (negative) number.In that case, it represent how many character to
   drop on each side. If it is a char, strip until (from) the given
   char.
-  ``%format`` integer formatting (as in
   `printf <https://hackage.haskell.org/package/base-4.18.0.0/docs/Text-Printf.html>`__)

For example, given box inh shelf ``E01.03/2``, and ``#top`` is present

::

   $[top?]  => top
   $[bottom?]  => ''
   $[top?on]  => on
   $[bottom?Y:N]  => N
   $[top??on top]  => top
   $[bottom??at the bottom]  => at the bottom
   ${shelfname} => E01.03/2
   ${shelfname:.} => 03/2
   ${shelfname::/} => E01.03
   ${shelfname:.:/} => 2
   ${shelfname:2} E01.03
   ${shelfname:-2} /2
   $[@global%05d] 00100

This can be used in ordering as well.
::rST -}
parseEvaluator :: Text -> (Text, [Text] -> Maybe Text)
parseEvaluator tag0 | Right (tag, elseValue) <- P.parse parser (unpack tag0) tag0 = 
  ( tag 
  , \case
      [] -> elseValue
      vs -> Just $ intercalate ";" vs
  )
  where parser :: MParser (Text, Maybe Text)
        parser = do
          tag <- P.takeWhileP Nothing (/= '?')
          P.string "??"
          elseValue <- P.optional ( P.takeRest )
          return (tag,  elseValue)
-- value?then:else
parseEvaluator tag0 | Right (tag, thenValue, elseValue) <- P.parse parser (unpack tag0) tag0 = 
  ( tag 
  , \case
      [] -> elseValue
      _ -> thenValue <|> Just tag
  )
  where parser :: MParser (Text, Maybe Text, Maybe Text)
        parser = do
          tag <- P.takeWhileP Nothing (/= '?')
          P.char '?'
          thenValue <- P.optional (P.takeWhile1P Nothing (/= ':'))
          elseValue <- P.optional ( P.char ':' >> P.takeRest )
          return (tag,  thenValue, elseValue)

-- value??def
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
  where parser :: MParser (Text, Maybe Text, Maybe Text)
        parser = do
          tag <- P.takeWhileP Nothing (/= ':')
          P.char ':'
          startm <- P.optional (P.takeWhile1P (Just "start index") (/= ':'))
          endm <- P.optional do
            P.char ':' >> P.takeRest
          return (tag, startm, endm)
parseEvaluator tag0 | Right (tag, format) <- P.parse parser (unpack tag0) tag0 =
  let f ::  Int -> Text
      f =  pack . printf format
  in ( tag
     , \case
        [] -> Nothing
        values -> Just . f $ sum $ mapMaybe readMay values
     )
  where parser :: MParser (Text, String)
        parser = do
           tag <-  P.takeWhileP Nothing (/= '%')
           format <- P.char '%' >> P.takeRest
           return (tag, '%': unpack format)
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

{- rST::break
Normally, when moves boxes to shelves, shelves are filled continuously
without any break betweenboxes of the same dimensions. Breaks can be
introduced using special break tags. By tagging a given box with a break
tag, the box is guaranteed to either start a new shelf, a new slice (row
or column depending of the shelf filling strategy) or a new slot (no box
"behind"). The corresponding tags are

-  ``@start=new-shelf`` first box of an entire shelf
-  ``@start=new-slice`` first box of a row/column
-  ``@start=new-slot`` no box behind
::rST -}
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
extractTags name = (style, maybe [] (splitOnNonEscaped "#") tagM) where
  (style, tagM) = extractTag name


withBoxOrientations :: [OrientationStrategy] -> WH a s -> WH a s
withBoxOrientations strats = withBoxOrientations' [(Nothing, strats)]
  
withBoxOrientations' :: [(Maybe ShelfSelector, [OrientationStrategy])] -> WH a s -> WH a s
withBoxOrientations' [] action = action
withBoxOrientations' (r:rs) action = withBoxOrientations' rs $ go r action where
  go (_, []) action =  action
  go (selectorm, strats) action = do
     oldStrategy <- gets boxOrientations
     let newStrategy = case selectorm of
                         Nothing -> \_ _ -> strats
                         Just  ShelfSelector{..} ->
                            \box shelf -> if applySelector sBoxSelectors box
                                             && applySelector sShelfSelectors shelf
                                          then strats
                                          else oldStrategy box shelf
     modify \wh -> wh { boxOrientations = newStrategy }
     action <* modify \wh -> wh { boxOrientations = oldStrategy }


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
  case splitOnNonEscaped ("+") offsets of
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
      (nl: nw: nh:_) = map readMay (splitOnNonEscaped ":" pos) ++ repeat Nothing
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
  (l, cs, h) = case splitOnNonEscaped "x" limits of
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

extractModes :: Text -> (Text, (ExitMode, Maybe PartitionMode, AddOldBoxes, Maybe SortBoxes))
extractModes modeLoc = case P.parse ((,) <$> modesParser <*> P.getInput) (unpack modeLoc) modeLoc of
  Left _ -> error "The unexpected happend. Contact your administrator" -- parser should succeed
  Right (r,e) -> (e,r)
modesParser :: MParser (ExitMode, Maybe PartitionMode, AddOldBoxes, Maybe SortBoxes)
modesParser = do
  exitMaybe <- P.optional (const ExitOnTop <$> P.char '^')
  es <- P.many (fmap Left partitionModeParser <|> fmap Right boxesP)
  let (parts, boxes) = partitionEithers es
      (oldBoxes, sortBoxes) = fromMaybe boxesDefault (headMay boxes)
      partitionMode = case parts of
                      [] -> Nothing
                      _:_ -> Just $ foldr1Ex POr parts
  return ( fromMaybe exitDefault exitMaybe
         , partitionMode
         , oldBoxes
         , sortBoxes
         )
  where
        go (p, v) = const v <$> P.try p
        boxesP = asum $ map go
                [ (P.string "&", (NewBoxesOnly, Just DontSortBoxes))
                ]
                <> map go
                [ (P.char '@', (AddOldBoxes, Just SortBoxes))
                , (P.char '+', (AddOldBoxes, Just DontSortBoxes))
                ]
        exitDefault = ExitLeft
        boxesDefault = (NewBoxesOnly, Nothing)

partitionModeParser :: MParser PartitionMode
partitionModeParser = do
  parts <- P.some partitionP
  return case parts of
           [] -> error "the unexpected happened" -- use of some
           _:_ -> foldr1Ex POr parts

  where partitionP = asum $  parseCorner
                          : map go
                          [ (string "right", PRightOnly)
                          , (string "above", PAboveOnly)
                          , (string "best", PBestEffort)
                          , (string "overlap", POverlap OLeft)
                          , (P.string "-->", POverlap ORight)
                          , (P.string "--|", POverlap OAligned)
                          , (string "oright", POverlap ORight)
                          , (string "oaligned", POverlap OAligned)
                          , (P.string "--^", PSortedOverlap)
                          , (string "sorted", PSortedOverlap)
                          , (string "behind", PBehind)
                          ]
                          <> map go
                          [ (P.char '~', PAboveOnly)
                          , (P.char ':', PRightOnly)
                          , (P.char '%', PBestEffort)
                          , (P.char '-', POverlap OLeft )
                          , (P.char '_', PBehind)
                          ]
        go (p, v) = const v <$> P.try p
        parseCorner = do
          "corner"
          n <- P.decimal
          P.optional (P.char ',')
          return $ PCorner n
        string s = s <* optional (P.char ',')
  
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
  
-- | Create a new even only if the previous one is not NoHistory
-- or level is 0.
-- Effectively , setting a 0 level will activate history
newWHEvent :: Int -> Text -> WH () s
newWHEvent level title = do
    wh <- get
    -- | if 
    let new = newEvent (level) (whCurrentEvent wh) title
        current = whCurrentEvent wh
    if current == NoHistory && level > 0 
    then return ()
    else 
      put wh { whCurrentEvent = new, whEventHistory = new : whEventHistory wh } 

newBaseEvent :: Text -> Text -> WH () s
newBaseEvent header = newWHEvent baseLevel . addHeader header where
   addHeader "" t = t
   addHeader h t = h <> ": " <> t
