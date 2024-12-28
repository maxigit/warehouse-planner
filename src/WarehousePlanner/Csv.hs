{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Miscelaneous functions to read and write shelves and boxes from csv
module WarehousePlanner.Csv 
( extractModes 
, expand
, parseOrientationRule
, readBoxes
, readClones
, readDeletes
, readLayout
, readMoves
, readMovesAndTags
, readShelfJoin
, readShelfSplit
, readShelfTags
, readShelves
, readStockTake
, readTags
, readTransformTags
, readUpdateShelves
, readCheckShelves
, readWarehouse
, readColourMap
, readRearrangeBoxes
, readFreezeOrder
, setOrientationRules
) where


import WarehousePlanner.Base
import WarehousePlanner.ShelfOp
import WarehousePlanner.Rearrange
import WarehousePlanner.Check
import WarehousePlanner.Selector
import WarehousePlanner.Move
import Data.Csv qualified as Csv
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as BS
import Data.Vector qualified as Vec
import Data.Map qualified as Map
import Control.Monad hiding(mapM_,foldM)
-- import Data.List.Split (splitOn)
import Data.List qualified as List
import ClassyPrelude hiding(readFile)
import Control.Monad.State hiding(fix,mapM_,foldM)
import Text.Regex qualified as Rg
import Text.Regex.Base.RegexLike qualified as Rg
import Data.Text(splitOn)
import Data.Text.IO(readFile)
import GHC.Utils.Monad (mapAccumLM)
import System.FilePath.Glob qualified as Glob
import Data.List.NonEmpty (NonEmpty, nonEmpty)

--  | Read shelves and allow formula between different shelves
--  example A,,100,200,100,
--          B,,100,250-{A},{A}
--  by default we use the same field as the current one of a refered object.
--  In the previous example 250-{A} means "width of A"
--  Reference must be between bracket
--  Reference can also do basic subsitution with the current shelf name.
--  example for A123
--  {B%} -> B123
--  {B_+-} -> B132
readShelves :: BoxOrientator -> FilePath-> IO (WH [Shelf s] s)
readShelves defaultOrientator filename = do
    csvData <- BL.readFile filename
    -- Call Csv.decode but add default value to bottom if not present in header
    let decode = asum
          [ Csv.decode Csv.HasHeader csvData
          , fmap (Vec.map (\(name, description, l, w, h, shelfType) ->
                   (name, description, l, w, h, shelfType, "0"))
                 )
                 (Csv.decode Csv.HasHeader csvData)
          ]

    case decode of 
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
            v <- Vec.forM rows $ \(name, description, l, w, h, shelfType, bottom) -> do
                        let (dim, dim') = dimsToMinMax l w h
                            _types = description :: Text
                            (_shelfO, fillStrat) = case toLower (shelfType :: Text) of
                                "deadzone" ->  (AddOrientations [] [up, rotatedUp ], RowFirst)
                                "shelf" -> (ForceOrientations [tiltedForward, tiltedFR], ColumnFirst)
                                _ -> (defaultOrientator, ColumnFirst)
                            updateShelfWithFormula' d d' bot _boxo _strat name tags=
                              updateShelfWithFormula d d' bot name  tags
                              

                        (name'tagS, go) <-
                            if toLower shelfType == "update"
                            then do
                              names <- shelvesFromSelector name
                              return ( map (,[]) names , updateShelfWithFormula')
                            else
                              return ( expand =<< splitOnNonEscaped "|" name
                                     , newShelfWithFormula
                                     )
                        mapM (\(n, tags) ->
                            let r = dimFromRef n
                            in go
                                    (dimToFormula sMinD r dim)
                                    (dimToFormula sMaxD r dim')
                                    (bottomToFormula n bottom)
                                    defaultOrientator
                                    fillStrat
                                    n
                                    (case tags of 
                                       [] -> Nothing 
                                       _ -> Just $ intercalate "#" tags)
                                    ) name'tagS

            return $ concat (Vec.toList v)

-- | a dimension can represent either the one from minDim, maxDim or both
-- syntax is minDim;maxDim  if ; not present use it for both
dimsToMinMax :: Text -> Text -> Text -> ((Text, Text, Text), (Text, Text, Text))
dimsToMinMax l w h = 
    let dim = (,,) lmin wmin hmin
        dim' = (,,) lmax wmax hmax
        [(lmin, lmax), (wmin,wmax), (hmin,hmax)] = map toMinMax [l, w, h]
        toMinMax d = case splitOnNonEscaped ";" d of
                         dmin: dmax: _ -> (dmin, dmax)
                         _ -> (d, d)
    in (dim, dim')

shelvesFromSelector name = do
    let (BoxSelector boxSel shelfSel _) = parseBoxSelector name
        selector = ShelfSelector boxSel shelfSel
    shelfIds <- findShelvesByBoxNameAndNames selector
    shelves <- mapM findShelf shelfIds
    return $ map shelfName shelves

readUpdateShelves :: FilePath-> IO (WH [Shelf s] s)
readUpdateShelves filename = do
  csvData <- BL.readFile filename
  let decode =  asum
          [ Csv.decode Csv.HasHeader csvData
          , fmap (Vec.map (\(name, l, w, h)->
                     (name, l, w, h, "",Nothing))
                 ) (Csv.decode Csv.HasHeader csvData)
          , fmap (Vec.map (\(name, l, w, h,tag)->
                     (name, l, w, h, "",tag))
                 ) (Csv.decode Csv.HasHeader csvData)
          ]

  case decode of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
          v <- Vec.forM rows $ \(name, l, w, h, bottom,tag) -> do
            let (dim, dim') = dimsToMinMax (def l) (def w) (def h)
                def "" = "{%}"
                def t = t
            names <- shelvesFromSelector name
            mapM (\n -> 
              let r = dimFromRef n
              in 
                updateShelfWithFormula 
                  (dimToFormula sMinD r dim)
                  (dimToFormula sMaxD r dim')
                  (bottomToFormula n (def bottom))
                  n 
                  tag
              ) names
          return $ concat (Vec.toList v)
              

          

readCheckShelves :: FilePath -> IO (WH () s)
readCheckShelves filename = do
    csvData <- BL.readFile filename
    case Csv.decode Csv.HasHeader csvData of
       Left err  -> error $ "File:" <> filename <> " " <> err
       Right (rows) -> return do
          void $ Vec.forM rows \(Csv.Only selector) -> do
             shelves <- findShelvesByBoxNameAndNames selector
             mapM_ tagBoxesStatus shelves
              


-- | Expand chars (or words) between bracket to all possible string
-- example:
--    A[135] => A1 A3 A5
--    A[13 5] => A13 A5
-- if a tag is present at the end
-- will be added to the last element
-- example:
--   A[123#top] => A1 A2 A3#top
-- If the string contains some spaces, the string will
-- be using (space separated) words instead of chars
expand :: Text -> [(Text, [Text])]
expand name = let
  (fix, vars0) = break (=='[') name
  in case vars0 of
    "" -> [extractTags fix]
    (uncons -> Just ('[', vars)) -> case break (==']') vars of
        (_,"") -> error $ "unbalanced brackets in " ++ unpack name
        (elements'tag, rest) -> do -- [ ... ]
              let (element'tagss, lasttag) =
                   case words elements'tag of
                        [one] -> -- each char has to be expanded
                                 -- a tag if present has to be at the end 
                                 let (chars,t) = extractTags one
                                 in (map (\c -> (singleton c, [])) $ toList chars, t)
                        es -> (map extractTags es, [])
              let n  = length element'tagss
              ((e, tags) ,i) <- zip element'tagss [1..n]
              (expanded, exTag) <- expand (drop 1 rest)
              let finalTags = concat [tags, if i == n then lasttag else [], exTag]
              return (fix <> e <> expanded , finalTags)
    _ -> error "Should not happen" -- We've been breaking on [


-- | Update an existing shelf
updateShelfWithFormula :: (WH Dimension s) -> (WH Dimension s) -> (WH Double s) -> Text -> Maybe Text ->  WH (Shelf s) s
updateShelfWithFormula dimW dimW' bottomW name tagm = do
  shelfIds <- findShelfBySelector (Selector (NameMatches [MatchFull name]) [])
  case shelfIds of
    [shelfId] -> do
        shelf <- findShelf shelfId
        dim <- dimW
        dim' <- dimW'
        bottom <- bottomW
        new <- updateShelf (\s -> s { minDim = dim, maxDim = dim', bottomOffset = bottom }) shelf
        let extraTags = (if minDim shelf /= dim
                         then [ "'debug-minShelf=+" <> printDim (minDim shelf) ]
                         else []
                        ) ++
                       ( if maxDim shelf /= dim'
                         then ["'debug-maxShelf=+" <> printDim (maxDim shelf) ]
                         else []
                       ) ++
                       ( if bottomOffset shelf /= bottom 
                         then ["'debug-bottom=+" <> tshow (bottomOffset shelf) ]
                         else []
                       )
        let tags = toList tagm <> (if tagIsPresent shelf "debug" then extraTags else [])
        case tags of
          [] -> return new
          _ -> do
              let tagOps = parseTagOperations =<< tags
              updateShelfTags tagOps new
    [] -> error $ "Shelf " <> unpack name <> " not found. Can't update it"
    _ -> error $ "To many shelves named " <> unpack name <> " not found. Can't update it"

-- | Read a csv describing how to split a shelf
-- The user basically gives a new length, width, and depth
-- This create a new shelf with the given dimensions
-- and cut it out (using guillotin cut) of the existing shelf
-- If a set of box is used, the dimension of the first box
-- can be used in formula
readShelfSplit :: FilePath -> IO (WH [Shelf s] s)
readShelfSplit = readFromRecordWith go where
  go (style, location, l, w, h) = do
    let locations = splitOn "|" location
    boxes <- findBoxByNameAndShelfNames style >>= mapM findBox
    shelfIds <- findShelfBySelectors (map parseSelector locations)
    shelves <- mapM findShelf shelfIds
    let boxm = headMay boxes
        withD s = if null s then "{}" else s :: Text
    concat `fmap` mapM (\shelf -> do
      [ls, ws, hs] <- zipWithM (\xtext f -> 
                            mapM (\x -> do
                              evalExpr (dimForSplit boxm shelf)
                                       (parseExpr (f . sMinD)  $ withD x)
                              ) (splitOnNonEscaped " " xtext)
                            ) [l, w, h] ds
      splitShelf shelf ls ws hs
      ) shelves


-- | Join shelves which have been previously split.
readShelfJoin :: FilePath -> IO (WH [Shelf s] s)
readShelfJoin = readFromRecordWith go where
  go (Csv.Only location) = do
    let locations = splitOnNonEscaped "|" location
    shelves <- findShelfBySelectors (map parseSelector locations) >>= mapM findShelf
    mapM unSplitShelf shelves


-- | Read a csv described a list of box with no location
-- boxes are put in the default location and tagged with the new tag
readBoxes :: [Text] -> [Orientation] -> (Text -> (Text, Text)) -> FilePath -> IO (WH [Box s] s)
readBoxes tagOrPatterns boxOrientations splitter filename = do
    csvData <- BL.readFile filename

    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right rows -> return $ do
            let v = Vec.forM rows $ \(style', qty, l, w, h) -> do
                        let dim = Dimension l w h
                            _types = qty :: Int
                            (name, tags) = extractTags style'
                            (style, content) = splitter name
                        s0 <- incomingShelf

                        forM [1..qty] $   \_ -> newBox style content dim (headEx boxOrientations) s0 boxOrientations (readTagAndPatterns tagOrPatterns tags)

            concat `fmap` (Vec.toList `fmap` v)

-- | Read a csv file cloning existing boxes
-- This can be used to create ghosts, ie fake boxes
-- used to reserve some space. 
readClones :: [Text] -> FilePath-> IO (WH [Box s] s)
readClones tagOrPatterns filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right rows -> return $ do  -- IO
          cloness <- forM (Vec.toList rows) $ \p@(selector, qty, content'tags) -> do -- WH
                let (content0, tags) = extractTags content'tags
                    (copyTag, content1) = case stripPrefix "!" content0 of
                                           Nothing -> (False, content0)
                                           Just c  -> (True, c)
                    content2 = if null content1 then Nothing else Just content1
                s0 <- incomingShelf
                
                newBaseEvent "CLO" (tshow p)
                boxIds <- findBoxByNameAndShelfNames selector
                boxes <- mapM findBox boxIds
                let box'qtys =  [(box, q) | box <- boxes , q <- [1..qty :: Int]] -- cross product
                forM box'qtys  $ \(box, i) -> do
                    content <- mapM (expandAttribute box i) content2
                    newbox <- newBox (boxStyle box)
                            (fromMaybe (boxContent box) content) -- use provided content if possible
                            (_boxDim box)
                            (orientation box)
                            s0
                            (boxBoxOrientations box)
                            (if copyTag then filter (not . isPrefixOf "'") (getTagList box) else [])
                    updateBoxTags (parseTagAndPatterns tagOrPatterns tags)
                                  newbox  {boxTags = boxTags box} -- copy tags
                                  -- note that those tags are only used
                                  -- to expand attributes but are not 
                                  -- acutally set in the box, because 
                                  -- updateBoxTags update the tags of box found in the warehouse
                                  -- boxTags is therefore only set temporarily
                                  i
          return $ concat cloness

readDeletes :: FilePath-> IO (WH [Box s] s)
readDeletes filename = do
  content <- readFile filename
  return $ do -- IO
      boxess <- forM (lines content) $ \selector -> do -- WH
        newBaseEvent "DEL" selector
        boxes <- findBoxByNameAndShelfNames (parseBoxSelector selector)
        deleteBoxes boxes
      return (concat boxess)

-- | Split a file so
-- a|b|c d|e f   => [[[a,b,c] [d,e] [f]]]
-- Line starting with < will be reversed
-- < a|b|c => c b a
-- > stays as normal (just there so on can disable and reenable easily << by transforming then to >>
readLayout :: FilePath -> IO (Runs NonEmpty Text)
readLayout filename = do
    content <- readFile filename

    return $ fromRuns unsafeNonEmpty $  fmap (processLine) (filter (not . comment) $ lines content)
    where processLine (uncons -> Just ('<', line)) = reverse (processLine line)
          processLine (uncons -> Just ('>', line)) = processLine line
          processLine line = map (splitOnNonEscaped "|")  (words line)
          comment (uncons -> Just ('#',_)) = True -- line starting with #
          comment "" = True -- or empty line
          comment _ = False
          unsafeNonEmpty :: [a] -> NonEmpty a
          unsafeNonEmpty = fromMaybe (error $ show filename <> " contains null lines") . nonEmpty 

readColourMap :: FilePath -> IO (Map Text Text)
readColourMap filename = do
    csvData <- BL.readFile filename
    case Csv.decode Csv.HasHeader csvData of
       Left err -> error $ "File:" <> filename <> " " <> err
       Right rows -> return $ mapFromList $ Vec.toList rows

    
readWarehouse :: FilePath -> IO (WH (RunsWithId s) s)
readWarehouse filename = buildWarehouse `fmap` readLayout filename


-- | read a file assigning styles to locations
-- returns left boxes
readMoves :: [Text] -> FilePath -> IO ( WH [Box s] s)
readMoves tagOrPatterns = readFromRecordWithPreviousStyle (\style (Moves location orientations) -> processMovesAndTags tagOrPatterns (style, [], Just location, orientations))

-- | Hack to allow different csv format
data Moves s = Moves  Text [OrientationStrategy]
instance Csv.FromRecord (Moves s) where
        parseRecord v = (\(Csv.Only location) -> Moves location []) <$> Csv.parseRecord v
                        <|>
                        (\(location, orientations) -> Moves location $ parseOrientationRule [tiltedForward, tiltedFR] orientations) <$> Csv.parseRecord v
         

readFromRecordWith :: Csv.FromRecord r => (r -> WH [a s] s) -> FilePath -> IO (WH [a s] s)
readFromRecordWith  rowProcessor filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> return $ do
          v <- Vec.forM rows rowProcessor
          return $ concat (Vec.toList v)
          
data BoxSelectorPlus r = SetPrevious BoxSelector BoxSelector r
                       | UsePrevious BoxSelector r
     deriving (Show)

instance Csv.FromRecord r => Csv.FromRecord (BoxSelectorPlus r) where
        parseRecord v = do
            let open = "&["
                close = "]&"
            let split bs = case BS.breakSubstring open bs of
                             (before, "") -> case BS.breakSubstring close before of
                                                 (_middle, "")  -> (before,"","") -- "before"
                                                 (middle, after) -> ("", middle, drop 2 after) -- "middle&]after"
                             (before, bs') -> case BS.breakSubstring close (drop 2 bs') of
                                                 (middle, after) -> (before, middle, BS.drop 2 after)
            case uncons v of
              Just (f, v') -> do
                s <- Csv.parseField f
                record <- Csv.parseRecord v'
                boxp <- case split  (s :: ByteString) of
                      (_before, "", "") -> UsePrevious  <$> Csv.parseField s <*> return record
                      (before, ref, after) -> SetPrevious <$> Csv.parseField ref
                                                          <*> Csv.parseField (before <> ref <> after)
                                                          --                 ^
                                                          --                 |
                                                          --                 +- string without &[ &]
                                                          <*> return record
                return boxp
              Nothing -> fail "<not box selector plus>"
                    
-- | Like readFromRecocddWith  but allow the BoxSelector to
-- be saved and reused in the next lines
readFromRecordWithPreviousStyle :: Csv.FromRecord r => (BoxSelector -> r -> WH [a s] s) -> FilePath -> IO (WH [a s] s)
readFromRecordWithPreviousStyle rowProcessor filename = do
    csvData <- BL.readFile filename
    case Csv.decode Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right rows -> return $ do
          b'v <- mapAccumLM process selectAllBoxes (Vec.toList rows)
          return $ concat $ snd b'v
    where process previous  boxSelectorPlus =
            let (next, sel, row) = case boxSelectorPlus of
                 SetPrevious prev boxs row -> (prev, boxs, row)
                 UsePrevious boxs row ->  (previous, previous `merge` boxs, row)
            in (next,) <$> rowProcessor sel row

          merge previous sel =
             BoxSelector (boxSelectors previous `merges` boxSelectors sel)
                         (shelfSelectors previous `merges` shelfSelectors sel)
                         (numberSelector previous `mergen` numberSelector sel)
          merges (Selector namep tagsp) (Selector namep' tags) = Selector (mergeNames namep namep') (tagsp <> tags) 
          mergen p (BoxNumberSelector NoLimit NoLimit NoLimit) = p
          mergen _ bn = bn
          -- merge easy ones
          mergeNames AnyNames ns = ns
          mergeNames ns AnyNames = ns
          mergeNames (NameMatches [MatchFull full]) (NameMatches matches) = let
            fullGlob = Glob.compile $ unpack full
            addFull (MatchFull f) = MatchFull (full <> f)
            addFull (MatchAnything) = MatchFull full
            addFull (MatchGlob glob) = MatchGlob (fullGlob <> glob)
            in NameMatches (map addFull matches)
          mergeNames (NameMatches [m@(MatchGlob fullGlob)]) (NameMatches matches) = let
            addFull (MatchFull full) = MatchGlob (fullGlob <> Glob.compile (unpack full))
            addFull (MatchAnything) = m
            addFull (MatchGlob glob) = MatchGlob (fullGlob <> glob)
            in NameMatches (map addFull matches)
          mergeNames _ names = names
            
-- | Move and tag boxes.
-- If a location (destination) is provided, only boxes which have been moved will be tagged -
-- leftovers (boxes not fitting ) will be untagged boxes which haven't
-- That way, a tag can either be set on succesfully moved boxed
-- or set on failure (by providing a negative tag).
-- This tagging/untagging ensure that after a move only the moved boxes
-- have the expecting tag (regardless of the previous tags on the boxes)
-- Locations separated by " " will processed in sucession with the leftover
-- so that ^A|B C|D is equivalent to
--     ^A|B#moved
--     #-moved,^C|D.
-- This normally doesn't change anything in ExiftLeft mode
-- but in ExitTop mode make sure the same set of shelves is reuse
-- before going to the left
-- example ^A|B|C|D will exit on top of B  and fill C (even though
-- there might be space left to create a new column in A)
-- \^A|B C|D will exit B to A  until A and B are full
-- Tag can be exclude using a glob pattern
-- This is to allows script to import partial tagging.
processMovesAndTags :: [Text] -> (BoxSelector, [Text], Maybe Text, [OrientationStrategy]) -> WH [Box s] s
processMovesAndTags tagsAndPatterns param = do
  inEx <- moveAndTag withAll tagsAndPatterns param 
  return (includedList $ fmap fst inEx)

-- | read a file assigning tags to styles
-- returns left boxes
readTags :: [Text] -> FilePath -> IO ( WH [Box s] s)
readTags tagOrPatterns = readFromRecordWithPreviousStyle (\style (Csv.Only tag) -> processMovesAndTags tagOrPatterns (style, splitOnNonEscaped "#" tag, Nothing, []))

-- | Hack to allow different csv format
data ForMovesAndTags s = ForMovesAndTags  Text [OrientationStrategy]
instance Csv.FromRecord (ForMovesAndTags s) where
        parseRecord v = (\(Csv.Only tag) -> ForMovesAndTags tag []) <$> Csv.parseRecord v
                        <|>
                        (\(tag, orientations) -> ForMovesAndTags tag $ parseOrientationRule [tiltedForward, tiltedFR] orientations) <$> Csv.parseRecord v

-- | Read tags or moves. Normally we could consider
-- that by default, we have a location, unless we start with '#'
-- and the it's a tag. However, location can have a tag. They need
-- then to be prefixed by /
-- #tag
-- location
-- /#taggedLocation
-- /location
-- #tag/location
-- location,tag
readMovesAndTags :: [Text] -> FilePath -> IO (WH [Box s] s)
readMovesAndTags tags0 = readFromRecordWithPreviousStyle go where
  go style (ForMovesAndTags tag'location orientations) =
    let (tags, locM) = splitTagsAndLocation tag'location
    in processMovesAndTags tags0 (style, tags, locM, orientations)



-- * Read Rearrange Boxes
readRearrangeBoxes :: [Text] -> FilePath -> IO (WH [Box s] s)
readRearrangeBoxes tags'Sticky = readFromRecordWithPreviousStyle go
  where go style (Csv.Only action) = do
           let (deleteUnused, grouping, actions) = parseActions action
           newBaseEvent "RAR"  action
           rearrangeBoxesByContent debugm deleteUnused grouping tagOps isUsed isSticky style actions
        (tags0, drop 1 -> sticky) = break (== "@sticky") tags'Sticky
        (deadm, tags1) = extractTagValue ("@dead") tags0
        (debugm, tags2) = extractTagValue ("@debug") tags1
        tagOps = parseTagAndPatterns tags2 []
        isUsed = not . flip tagIsPresent (fromMaybe "dead" deadm)
        isSticky = (`List.elem` sticky)
        
extractTagValue :: Text -> [Text] -> (Maybe Text, [Text])
extractTagValue needle tags0 = let
  (values, tags) = partitionEithers $ map (\t -> maybe (Right t) Left $ stripPrefix (needle <> "=") t) tags0
  in (headMay values, tags)
-- * Freeze order  
readFreezeOrder :: [Text] -> FilePath -> IO (WH [Box s] s)
readFreezeOrder tags0 = readFromRecordWith go
  where go (Csv.Only style) = do
          boxes <- findBoxByNameAndShelfNames style
          freezeOrder $ map boxId boxes
          case parseTagAndPatterns tags0 [] of
            [] -> return boxes
            ops -> zipWithM (updateBoxTags ops) boxes [1..]


-- * Read Shelf Tags 
readShelfTags :: FilePath -> IO (WH [Shelf s] s)
readShelfTags = readFromRecordWith go where
  go (selector, splitOnNonEscaped "#" -> tags) = do
    shelves <- findShelvesByBoxNameAndNames selector
    let tagOps = map parseTagOperation tags
    mapM (updateShelfTags tagOps) shelves
-- * Read transform tags 
-- | Temporary type to read a regex using Cassava
-- Usefull so that regex validation is done when reading the file
type RegexOrFn s =  Either Rg.Regex (Box s -> Int -> WH Rg.Regex s)
instance Csv.FromField (Either Rg.Regex (Box s -> Int -> WH Rg.Regex s)) where
  parseField s = do
    r <- Csv.parseField s
    case expandAttributeMaybe r of
      Nothing -> Left <$> Rg.makeRegexM (unpack r)
      Just _ -> return . Right $ \box i -> do
              expandAttribute box i r >>= Rg.makeRegexM . unpack

instance Csv.FromField BoxSelector where
  parseField s = do
    x <- Csv.parseField s
    return $ parseBoxSelector x
    
instance Csv.FromField ShelfSelector where
  parseField s = do
    x <- Csv.parseField s
    return $ parseShelfSelector x

instance Csv.FromField (Selector a) where
  parseField s = do
    x <- Csv.parseField s
    return $ parseSelector x

-- | Read transform tags
readTransformTags :: FilePath -> [Text] -> IO (WH [Box s] s)
readTransformTags path tags = readFromRecordWith (\(style, tagPat, tagSub) -> transformTags tags style tagPat tagSub) path

 -- -| Apply {transformTagsFor} to the matching boxes
 -- if tags are given only the given tags will be process.
 -- If only one tag is given, the substition will only apply
 -- to the tag value instead of the chain #tag=value
transformTags :: [Text] -> BoxSelector -> RegexOrFn s -> Text -> WH [Box s] s
transformTags tags style tagPattern tagSub = do
  boxes0 <- findBoxByNameAndShelfNames style
  boxes <- mapM findBox boxes0
  catMaybes <$> zipWithM (transformTagsFor tags tagPattern tagSub) boxes [1..]
  
-- | Regex tags substitutions. Each tags is matched and applied separately
-- The tag is not removed. To do so add a `#-\0` at the end
transformTagsFor :: [Text] -> RegexOrFn s -> Text -> Box s -> Int -> WH (Maybe (Box s)) s
transformTagsFor tags tagPat' tagSub box index = do
  tagPat <- either return (\f -> f box index) tagPat'
  let tagOps = case tags of
                [] -> transformTags (const True)
                [tag] -> transformTag tag
                _ -> transformTags (`elem` tags)
      transformTags keep =
          map parseTagOperation $
               concatMap (splitOnNonEscaped "#" . (\t -> pack $ Rg.subRegex tagPat (unpack t) (unpack tagSub)))
               (getTagList $ Map.filterWithKey (\k _ -> keep k)  $ boxTags box)
      transformTag tag = let
          values = getTagValues box tag
          in case values of
                  [] -> []
                  vs -> case filter (not . null) $ map (\t -> Rg.subRegex tagPat (unpack t) (unpack tagSub)) vs of
                        [] -> [(tag, RemoveTag)]
                        news -> [(tag, SetValues $ map pack news)]
  Just <$> updateBoxTags tagOps box index

-- | Read box dimension on their location
readStockTake :: [Text] -> [Orientation] -> (Text -> (Text, Text)) -> FilePath -> IO (WH ([Box s], [Text]) s)
readStockTake tagOrPatterns0 newBoxOrientations splitStyle filename= do
    let (unique, tagOrPatterns) = partition ("@unique=" `isPrefixOf`) tagOrPatterns0
        lookupWH :: forall z . WH ([Text] -> Maybe (Box z)) z
        lookupWH = case unique of 
                  (x:_) | Just by <- stripPrefix "@unique=" x -> do
                      boxMap <- getOrCreateBoxTagMap by
                      let byEqual = by <> "="
                      return \tags -> case mapMaybe (stripPrefix $ byEqual ) tags  of
                                       (value:_) -> lookup value boxMap >>= headMay
                                       _ -> Nothing
                  _ -> return $ const Nothing
    readStockTakeWithLookup lookupWH tagOrPatterns newBoxOrientations splitStyle filename

-- | Create new boxes unless a lookup function is given. In that case, lookup if the box already exists
-- and modify it. Usefull to merge to diff two warehouses.
readStockTakeWithLookup :: (WH ([Text] -> Maybe (Box s)) s)  ->  [Text] -> [Orientation] -> (Text -> (Text, Text)) -> FilePath -> IO (WH ([Box s], [Text]) s)
readStockTakeWithLookup lookupM tagOrPatterns newBoxOrientations splitStyle filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left _ ->  case Csv.decode Csv.HasHeader csvData of
                        Left err -> error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
                        Right rows -> return $ do
                              uniqueMap <- lookupM 
                              processStockTakeWithPosition uniqueMap tagOrPatterns newBoxOrientations splitStyle $ Vec.toList rows
        Right (rowsV) -> return $ do
            -- we get bigger box first : -l*w*h
            let rows0 = [ ( (qty, content, tags)
                          ,  ( -(l*w*h)
                             , shelf, style'
                             , l,w,h
                             , if null os then "%" else os
                             )
                          )
                        | (shelf, style, qty, l, w, h, os) <- Vec.toList (rowsV ::  Vec.Vector (Text, Text, Int, Double, Double, Double, Text))
                        , (styleAndContent, tags) <- expand style
                        , let (style', content) = splitStyle styleAndContent
                        ]
                -- dont sort if any of the shelves specification specify not no sort
                dontSort = not $ null  [ True
                                       | (_, (_,shelf,_
                                             ,_,_,_
                                             ,_
                                             ) 
                                         ) <- rows0
                                       , let (_, ( _,_,_, sortM)) = extractModes shelf
                                       , sortM == Just DontSortBoxes
                                       ]
                                       
            -- groups similar
                groups = List.groupBy (\a b -> snd a == snd b)
                       $ ( if dontSort then id else List.sortBy (comparing snd))
                       rows0

            (_,v) <- mapAccumLM  ( \_previousMatch rows@((_, (_,shelf, style, l, w, h, os)):_) -> do
                        s0 <- defaultShelf
                        lookup_ <- lookupM
                        let dim = Dimension l w h
                            (orStrategies, boxOrs) = 
                               -- don't override orientation rules
                               -- if given orientation are just 
                               case traverse readOrientationMaybe (toList os) of
                                    Just _ -> ([], readOrientations newBoxOrientations os)
                                    _ -> let strats = parseOrientationRule newBoxOrientations os
                                         in (strats, map osOrientations strats)
                        boxesS <- forM rows $ \((qty, content, tags),_) ->
                          forM [1..qty] $   \_ -> do
                            newBox' (lookup_ tags)
                                    style
                                    content
                                    dim
                                    (headEx boxOrs)
                                   s0
                                   boxOrs -- create box in the ERROR self
                                   (readTagAndPatterns tagOrPatterns tags)
                        let boxes = concat boxesS
                            -- pmode = POr PAboveOnly PRightOnly
                        leftOvers <- withBoxOrientations orStrategies do
                                         fmap fst . excludedList <$> moveToLocations withAll SortBoxes (map (,()) boxes) shelf

                        let errs = if not (null leftOvers)
                                      then map (\b -> unlines [ "ERROR: box " <> tshow b <> " doesn't fit in " <> shelf
                                                              -- , printDim dim <> " " <> (unwords $ map (printDim . maxDim) shelves)
                                                              ]
                                               ) leftOvers
                                      else []
                        return ( shelf
                               , (boxes, errs)
                               )
                        ) 
                        ""
                        groups
            let (boxes, errors) = unzip (v)

            return (concat boxes, concat errors)

-- | Like stocktake but put boxes at the given position (without checking overlapps)
-- or execute a list of FillCommands to place the box depending on the previous one
processStockTakeWithPosition :: ([Text] -> Maybe (Box s)) -> [Text] -> [Orientation] -> (Text -> (Text, Text)) -> [(Text, Text, Text, Double, Double, Double, Text)] -> WH ([Box s], [Text]) s
processStockTakeWithPosition lookupM tagOrPatterns newBoxOrientations splitter rows  =  do
  s0 <- defaultShelf
  let -- go :: Map Text FillState -> _ -> WH (FillState, _) s
      go (previousShelf, fillStateMap) (shelfname, posSpec, style', l, w, h, os) =  do
              let fillState = findWithDefault emptyFillState {fLastBox_ = Dimension l w h} shelfname fillStateMap
                  (name, tags ) = extractTags style'
                  (style, content) = splitter name
                  dim = Dimension l w h
                  boxOrientations = readOrientations newBoxOrientations os
              shelf <- if shelfname == fst previousShelf
                       then return $ snd previousShelf
                       else do
                        shelfs <- findShelfBySelector (Selector (matchName shelfname) [])
                        findShelf $ headEx $ shelfs ++ [s0]
              -- If not orientation is provided, use the best possible as if the shelf was empty
              let (bestOrientation, _, _) = bestArrangement [ OrientationStrategy orientation 1 1 Nothing Nothing False 
                                                            | orientation <- case boxOrientations of
                                                                                  [] -> newBoxOrientations
                                                                                  os -> os
                                                            ]
                                                            [(minDim shelf, maxDim shelf, ())]
                                                            dim
                  updatedTags = case lookupM tags of
                                  Nothing -> []
                                  Just old ->  ["@updated"] <> map ("_" <>) (readTagAndPatterns tagOrPatterns $ getTagList old)
                   
              box <- newBox' (lookupM tags)
                             style
                             content
                             dim
                             bestOrientation
                             (shelfId s0)
                             boxOrientations
                             (updatedTags <> readTagAndPatterns tagOrPatterns tags)
              let commandsE = case parsePositionSpec posSpec of
                                  Just (or, toPos) -> let pos = Position (toPos (_boxDim box)) or
                                                      in Right [FCBoxWithPosition box pos]
                                  Nothing -> case parseFillCommands posSpec of 
                                               -- [] | not (null posSpec) -> Left $ posSpec <> " is not a valid position."
                                               coms -> Right $ coms <> [FCBox box Nothing]
                  updateM st = Map.insert shelfname st fillStateMap
                  -- we need to set the lastBox to the current box unless we ignore the dimension
                  -- also if  "BOXDIM" is present insert the current box there
                  addBoxDimToCommands coms =           
                    let ignores = [ t | FCSetIgnoreDimension t <- coms ]
                        setCurrent = FCSetLastBox $ MakeDimension (\_ _ _ -> Dimension l w h) "current box"
                        isUseCurrentBox FCUseCurrentBox = True
                        isUseCurrentBox _ = False

                    in if fromMaybe (fIgnoreDimension fillState) (lastMay ignores)
                       then coms
                       else case break isUseCurrentBox coms of
                              (before, _:after) -> before ++ (setCurrent : after)
                              _ ->  setCurrent : coms
                     
              case commandsE of
                  Left e -> return $ (((shelfname, shelf) , updateM fillState), Left e)
                  Right commands -> do
                        (newState, boxems) <- mapAccumLM (executeFillCommand shelf) fillState $ addBoxDimToCommands commands
                        return $ ( ((shelfname, shelf), updateM newState)
                                 , Right $ catMaybes boxems
                                 )

  (_, boxeEs) <- mapAccumLM go (("", error "BOOM"), mempty) rows
  let (errors, boxes) = partitionEithers boxeEs
  return $ (concat boxes, errors)
      
      

-- * read orientation rules 
readOrientationRules :: [Orientation] -> FilePath -> IO (Box s -> Shelf s -> Maybe [OrientationStrategy])
readOrientationRules defOrs filename = do
    csvData <- BL.readFile filename
    case Csv.decode  Csv.HasHeader csvData of
        Left err ->  error $ "File:" <> filename <> " " <>  err -- putStrLn err >> return (return [])
        Right (rows) -> do
            let rules = fmap (\(boxSelectors, orientations) ->
                        let
                            ors = parseOrientationRule defOrs orientations
                            (BoxSelector boxSel shelfSel _) = parseBoxSelector boxSelectors
                            validate shelf box = if applySelector boxSel box
                                                    && applySelector shelfSel shelf
                                                 then Just ors
                                                 else Nothing
                        in validate
                        ) (Vec.toList rows)
                fn box shelf = case catMaybes $ [rule shelf box | rule <- rules ] of
                                  [] -> Nothing
                                  (result:_) -> Just result
            return fn
                           

setOrientationRules :: [Orientation] -> FilePath -> IO (WH () s)
setOrientationRules defOrs filename = do
  fn <- readOrientationRules defOrs filename

  return $ do
    old <- gets boxOrientations
    let new box shelf = case fn box shelf of
          Nothing ->  old box shelf
          Just or_ -> or_

    wh <- get
    put wh {boxOrientations = new}
    return ()
                              

-- orientationFromTag defOrs box shelf = let
--   fromTags = do -- []
--     tag <- boxTags box
--     parseOrientationRule tag
--   in
--   case fromTags of
--     [] -> map (,9) defOrs
--     _ -> fromTags

  
  
