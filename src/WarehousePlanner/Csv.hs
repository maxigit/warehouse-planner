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
import WarehousePlanner.Styling (Kolor, valueToKolorE)

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
{- rST::shelves 
Create or update set of shelves. CSV with the following header :

::

   name,comment,length,width,height,type[,bottom]

For ``lenght``, ``width`` and ``height`` a mix and a max can be
specified as follow *min*\ ``;``\ *max*. If ``;`` is not present the
same formula will be used for both dimensions. When update both
\_min\_ and \_max\_ or optional. The max of a dimension represents
the physical maximum that can be use and the min the maximum that a
box can be used after. For example when stacking boxes on the floor,
the maximum height will be the ceiling (all box stacks have to fit
under the ceiling) whereas the minimum height will the be maximum
height reachable to handle the box. In other word the last box can
stick out over min height, but no box will be stacked above the
minimum height even though it will fit under the maximum height.
``bottom`` is optional. It represents the height of where the usable
part of shelfstarts (taking the thickness of the shelf itself into
accoun). This is used to determine if a shelf sticks out of the
ceiling (when the ``ceiling`` tag is set). When using formula, the
value taken from the refered shelf will be the top of the shelf (not
the bottom). This is to allow the calculation of stacked shelves
using the

::

   {%-\}+10

formula (10 there is the thickness of the shelf)

Special shelves
'''''''''''''''

The two first declared shelves have a special meaning. The first one
is the ``error`` shelf and the second one is the ``pending`` shelf.
The error shelf is used when ever a move try to fit more boxses than
possible in the given shelf. All the selected boxes which doesn't fit
the given location are moved to the error shelf. The ``pending``
shelf is used when creating new boxes. Boxes created using the Boxes
command are created within the ``pending`` shelf. It is best practice
to name those shelves ``error`` and ``pending`` (or equivalent).

Name expansion
''''''''''''''

Groups of similar shelves can be created in one line using name
expansion. Before creating a shelf, the name is expanded into one or
many shelf names. One shelf will be created of each of the resulting
name. Shelf name are split with the ``|`` separator, then when
encountering ``[..]``, name will be expanded by generating a name for
each character (or word) within the bracket. Example

::

   A|B => A B
   A[01] => A0 A1
   A[ABx]X => AAX ABX AxX
   A[A Bx]X => AAX ABxX

More than one ranges can be provided, in this case all combinations
will be generated. Example

::

   [ABC][123] => A1 A2 A3 B1 B2 B3 C1 C2 C3

dimension formula
'''''''''''''''''

Shelf dimension depending on the dimension on another shelf can be
expressed using shelf dimension. This can be useful when shelf are
back to back or one shelf is not physically a shelf but the space
leftover between a shelf and wall. Basic arithmetic can be performed
and the the same dimension of another shelf can be referenced using
``{...}``. Wildcard (``_``,\ ``+`` and ``-``) can be used to modify
the corresponding character of the current shelf itsef. The minimum
and maximum of two values can be computed using the ``&`` and ``|``
operators. ``%`` can be used to match all characters (minus the
number of character after it)\ ``[``..\ ``]`` mirror the correspoding
character within the range.\ ``*`` matches all character up to the
nextExample

::

   name,comment,length,width,height,type
   A1,,150,40,200,
   A2,,100,{_-\},20, -- same width as A1 
   B1,,200-{A_},-- length 200 - length of A1
   B2,,200-{A_},-- length 200 - length of A2
   B20,,200-{A%},-- length 200 - length of A20
   C1,,200-{[AC]1},-- length 200 - length of A1
   C2,,{%1}, -- length of C1
   C3,,{%-\}, -- length of C2
   B21,,{*2-\}, -- lengh of B20 (*2 matches B2)

For A2, ``_`` refers to the first character of the current shelf,
i.e, ``A`` and ``-`` refers to the second character (``2``) minus 1
'> ``1`` For B1, the ``_`` is in the second position and therefore
correspond to the second character of ``B1`` : ``1``. For C1, the
``[AC]`` is transform into ``A`` (C->A, B->B, A->C) An accessor can
be added to a reference to select a particular dimension of the
refered object. This is done with ``{``\ ref\ ``:``\ accessor\ ``}``,
where accessor can be

'  ``length`` min length
'  ``width`` min width
'  ``height`` min height
'  ``Length`` max length
'  ``Width`` max width
'  ``Height`` max height
'  ``bottom`` bottom (height from the ground)
'  ``top`` top (height from the ground)
'  ``usedLength`` used length (depends on boxes within)
'  ``usedWidth`` used width
'  ``usedHeight`` used heigth
'  ``availableLength`` (min length ' used available)
'  ``availableWidth`` (min width ' used available)
'  ``availableHeight`` (min height ' used available)
'  ``AvailableLength`` (max length ' used available)
'  ``AvailableWidth`` (max width ' used available)
'  ``AvailableHeight`` (max height ' used available)

Please note, that ``l`` can be used for ``length``, ``al`` for
``availableLength``, ``AL`` for ``AvailableLength`` etc ...

shelf types
'''''''''''

The shelf type determines the default boxes orientation and filling
strategy. The current shelf style are

-  ``Shelf`` (normal first) : tilted row first
-  ``deadzone`` : allow up column first
-  ``Update`` allows to update an existing shelf (tags will be ignored)
-  ``other``, column first

Update
''''''

Shelves can be updated by redefining it and setting the type to
``Update``. Formulas can use the value of the shelf itself. This
feature allow to tweak a shelf previously defined within a group.
Note that for update, the shelf name is not expanded but filtered
using the normal selector syntax (*box*\ ``/``\ *shelf*).

Tag
'''

Tags can be used to select shelf when doing box moves, but is also
used to group shelves when displaying the summary. Shelves are
grouped using the ``summary`` property. Note that shelves with a
summary value starting with a ``_`` are considered as virtual shelves
and are not taken in to account when calculated used spaces and floor
space. Also, shelves with the ``sep`` tag are seen as separator :
shelves present for layout purpose only and are excluded from the
summary as well as being displayeddifferently.

Special Tag: Ceiling
''''''''''''''''''''

The special attribute ``ceiling=`` set the height of the ceiling. If
the total height + bottom offset is greater than the ceiling value,
the shelf is considered to high. It will be tagged with ``'tooHigh``,
and the height of the shelf will be truncated accordingly to fit the
given ceiling.
::rST -}
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

-- | Update shelves and allow update of current dimension using formulas.
{- rST::update-shelves
Update a set of shelves. CSV with the folowing header
::

   stock_id,l,w,h,bottom,tag

Updates the dimensions of the shelves containing selected boxes. Can
be used to readjust shelves and their neighbour according to the
space use by its content. Example

::

   :UPDATE_SHELVES:
   stock_id,l,w,h,bottom,tag
   /A,{A}+{B:availableLength},, -- expands A with B free space
   /B,{B:usedLength},, -- shrink B to its content

Expand A and shrink B by the same amount (so that A+B stays the
same),
::rST
-}
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
              

          

-- | Check the consistency of the given shelves.
{- rST::check-shelves

Check for "problems" in the given shelves. The status of boxes (overlap or stickout of shelves)
is given by tagging the checked boxes.

.. ihaskell:: Check::status

::rST -}
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
{- rST::shelf-split
Split a shelf performing guillotine cut. The dimension columns
specify the dimension to cut. it can be any formula with reference
another shelf or objet. For each object the dimension corresponding
to the column will be used, unless accessor is specified (see
`dimension formula <#dimension-formula>`__). If a box selector is
specified, the dimension of the first box found can be used.

.. ihaskell:: ShelfOp::shelf-split-formula

The split shelf is resized and the created ones have the same name
with a 3 letter suffix separated with ``/`` index added. Example

::

   :SHELF_SPLIT:
   stock_id,location,length,width,height
   ,A, {%}/2, , -- cut A in 2 of half the length : A A/baa
   ,A, {%}/4 {%}/2, 50, 10 -- cut A in 12 3x2x2 A A/baa A/caa A/bba A/cba ...
   ,A, {B:height},, -- cut length using shelf B height
   box,A,{|}*2,, cut at two time the lenght of box with | orientation
::rST -}
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
{- rST::shelf-join
Shelves which have been split can be join back together. The selector
must refer to the base shelf (not the split ones)

::

   :SHELF_SPLIT:
   stock_id,location,length,width,height
   ,A, {%}/2,{%}/2, -- create A/ba A/bb A/ab
   :END:
   :SHELF JOIN:
   location
   A -- join A/ba A/bb and A/ab to A
   :END:

::rST -}
readShelfJoin :: FilePath -> IO (WH [Shelf s] s)
readShelfJoin = readFromRecordWith go where
  go (Csv.Only location) = do
    let locations = splitOnNonEscaped "|" location
    shelves <- findShelfBySelectors (map parseSelector locations) >>= mapM findShelf
    mapM unSplitShelf shelves


-- | Read a csv described a list of box with no location
-- boxes are put in the default location and tagged with the new tag
{- rST::boxes
A set of boxes without initial location. They will be put in the
``pending`` shelf. It is a CSV with the following header :

::

   style,quantity,l,w,h

Tags provided in the section name, will be applied to the created
boxes.
::rST -}
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
{- rST::clones
Allows to duplicate the given boxes. Used in conjunction with
`Deletes` it can be used to do slotting by creating fake boxes
(ghosts) which will make sure a slot is full and the remove later.
For example

::

   :Clones:
   stock_id,quantity,content'tag
   A^1,4,#ghost

or (note the position of the tag ``ghost``

::

   :Clones-ghost:
   stock_id,quantity,tag
   A^1,4,

will create 4 boxes with the tag ``ghost`` for each colour of A.
``^1`` makes sure we are doing the cloning operation once per colour.
Without it, we will have 4 clones for every box.To create slots of
for, we could move all As by 4 with

::

   :Moves:
   stock_id,location
   A^4,destination

No more that 4 of each colour will be moved using the ghosts if
necessary. We can then delete the ghost using ``:Delete:``

::

   :Delete:
   A#ghost

The content of a new box can specified before the tag. For example

::

   :Clones:
   stock_id,quantity,content'tag
   A#'BLK^1,4,RED#ghost

Will create 4 red boxes for each BLK. By default only tags that are
specified either as default tag or for each line will be applied to
the box. To copy a box and ALL its tags, start the content/tag
specification with a ``!``.

::

   :Clones:
   stock_id,quantity,content'tag
   A^1,4,!#ghost
   A^1,4,!new-content
   A^1,4,!new-content#ghost
::rST -}
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
{- rST::delete
Delete the selected boxes.

::

   :Delete:
   A#ghost

Delete all ``A`` boxes with the ``ghost`` tag.

::rST -}
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
{- rST::layout
Describes how shelves should be displayed. Shelves are displayed as a
matrix (row and column) of bays. A bay being a set of shelves stacked
together from bottom to top. Each line of the layout section describe
a row. Columns are separated by one or more space and each element of
a bay by a pipe ``|`` Example:

::

   A1|A2|A3 B1|B2
   C D E

``A1|A2|A3`` form a bay of 3 shelves, A1 at the bottom, and A3 at the
top next to a bay of 2 shelves B1 with B2 on top.
::rST -}
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
{- rST::colour-map
Defines a map colour name to colour value. The value can be either a
existing colour name or a RGB value (without the ``#``). It is a csv
with the following header :

::

   name,value

Example

::

   :COLOURS:
   name,value
   red,ff0000
   good,green
   :END:
::rST -}
readColourMap :: FilePath -> IO (Map Text Kolor)
readColourMap filename = do
    csvData <- BL.readFile filename
    case Csv.decode Csv.HasHeader csvData of
       Left err -> error $ "File:" <> filename <> " " <> err
       Right rows -> let m = mapFromList $ Vec.toList rows
                      in case traverse valueToKolorE m of
                         Left e -> error $ unpack e
                         Right m' -> return m'


    
readWarehouse :: FilePath -> IO (WH (RunsWithId s) s)
readWarehouse filename = buildWarehouse `fmap` readLayout filename


-- | read a file assigning styles to locations
{- rST::moves
Describes a set of moves boxes to shelves. The first column describe
a set of boxes to moves to a set of shelves. If multiple shelves are
given, the Planner will fill the shelf in the given order and use the
optimal orientation. If all boxes can't fit the given shelves, the
excedendatary boxes will be moved to the **error** shelf. It is CSV
with the following header:

::

   stock_id,location[,orientations]

Please not the stock_id and location are in fact boxes and shelves
selectors (see selectors sections). An orientation can be given
optionnaly.

Filling order, Exit on top
''''''''''''''''''''''''''

When moves boxes to a new set of shelves, shelves are filled by
alphabetical order. For example the command

::

   :Moves:
   stock_id,location
   ,A|B|C

Will move all boxes to the shelves A, B and C starting by filling A,
the filling B and so on. Boxes are stacked in column form left to
right. It is however sometimes desirable to carry on filling the same
column on the next shelf rather than creating a new column on the
current shelf. This can be achieved by specifying the "exit on top"
option by starting the location with ``^``

::

   :Moves:
   stock_id,location
   ,^A|B|C

The code above, will fill the first colum into shelf A, then a column
in B and then C. When the first column in C is full, it will start a
2nd column in A, then B etc ... Separating shelves with `` `` will
indicate them as separate bay.

::

   :Move:
   :Moves:
   stock_id,location
   ,^A|B C

The code above, will fill the first columen in shelf A, then a column
in B and then restart in A and so on until there is not column left
in A and B. It will then start filling up C. (This syntax is similar
to the syntax of the **Layout** section).

.. _partition-mode:

Partition Mode
''''''''''''''

When filling a shelf with boxes, the default strategy is to use to
either fill the shelf on the right of the existing boxes or the top
(which ever gives the best result). This works fine most of the time
but might result in available spaces beeing "shadowed" by existing
corner. In the following configuration, ``#`` represents existing
boxes.

::

   |     .
   |a A  . B
   |##.......
   |##   . 
   |## C . D
   |######_d___

The default strategy will fill either d,D and B (filling at the
right) or a,A and B (filling on top). The C zone is shadowed. To put
a box in C, will requires to try every available rectangles which
will makes the planner very slow. However, if needed, the partition
mode (which parts of the shelf needs to be filled) can be specified
before the shelf name (as with "exit on top"). One or more partition
mode can be specified as follow: - ``~`` Above only (in the example
above: a A B) - ``:`` Right only (in the example above: d D B) -
``%`` Best effort (excluding above corners a and d) C A D B Not
specifying anything is equivalent to ``~:`` Another possibilty is to
empty the shelf(ves) and fill the shelves with the existing boxes and
the new ones. In that case, we might want to resort all boxes (old
and new) or keep them in the orignial order (old then new). - ``@``
Sort old and new boxes - ``+`` old then new boxes in original order
Example

::

   :Moves:
   stock_id,location
   ,%:~A -- tries a A B, A C B D and d D B
   ,A -- equivalent to ,~:A. Tries  a A B and d D B
   ,%:A -- tries A B C D and d D B
   #!,@A -- resort content of shelf A

Tagging
'''''''

Tags provided as section parameter will be applied to the boxes
**successfully** moved whereas boxes which couldn't be moved (not
enough space in the destination) will see those tags negated. For
example, let's say that we are trying to move 3 boxes in a shelf with
``:Moves_moved_-error`` but only the first 2 are moved successfully,
the two first boxes with see ``moved`` and ``-error`` applied (which
result in adding the tag moved but remove the tag error, whilst the
last box will see the ``-moved`` and ``error`` apply. As a result the
two first boxes will have the tag ``moved`` and the last one the tag
``error``.

Empty selection
'''''''''''''''

Sometimes, a selector doesn't select anything. This can be because of
a typo or because a box is not present anymore in the warehouse. To
detect such cases setting the tag ``@noEmpty`` will raise an error
(and stop) if there is nothing to moves.

::rST -}-- returns left boxes
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
{- rST::with-previous
When using csv file, part (or all) of a selector can be used as reference to be reused in
the following lines. A reference can be set by enclosing it between
``&[`` and ``]&``. It will be used as a base selector for all the
subsequent lines of the same sections. This is somehow equivalent to
select thoses boxes and then apply subsequent filter to the
"selection".

::

   &[A]&,action   -- set A as reference and apply action to all As.
   #tag,action2 -- equivalent to A#tag,action2
   ]&,B,action3 -- reset the reference and apply action3 to all Bs

The reference can only capture a part of the initial selector

::

   &[A]tag1,action   -- set A as reference and apply action to all As with tag1.
   #tag2,action2 -- equivalent to A#tag2,action2
   &[A#tag1]&,action   -- set A#tag1 as reference and apply action to all As with tag1.
   #tag2,action2 -- equivalent to A#tag1#tag2,action2

If only one delimiter is present, reference will set as follow

::

   &[A <=> &[A]&
   A]&  <=> &[]A&
   A&[extra  <=> A&[extra]&
   A]&extra  <=> &[A]&extra

This at the moment only works for the box selectors of the section
related to moving or tagging boxes.
::rST -}
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
            addFull (MatchOrd ord eq f) = MatchOrd ord eq (full <> f)
            addFull (MatchAnything) = MatchFull full
            addFull (MatchGlob glob) = MatchGlob (fullGlob <> glob)
            in NameMatches (map addFull matches)
          mergeNames (NameMatches [m@(MatchGlob fullGlob)]) (NameMatches matches) = let
            addFull (MatchFull full) = MatchGlob (fullGlob <> Glob.compile (unpack full))
            addFull pat@(MatchOrd _ _ _ ) = MatchGlob (fullGlob <> Glob.compile (unpack $ printPattern pat))
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
{- rST::tags
Tags allow boxes to be selected (via selector) to be either moved or
tagged but also change their behavior (colour, priority, etc ...) via
properties. A Tag can be removed by setting with ``-`` The body is a
CSV with the following header

::

    stock_id,tag

Example

::

   :TAGS:
   stock_id,tag
   ,#tag1
   A,#-tag1
   #tag1,#bg=red

The first line, tag all boxes with ``tag1``. The second line remove
``tag1`` from the A boxes. The last line set the background property
of the box tagged with ``tag1`` to red.


::rST -}-- returns left boxes
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
{- rST::moves-and-tags

.. _moves-and-tag:

Allows to move and tag at the same time a set of boxes. This can be
faster and less verbose than creating a move and a tag section. Tags
needs to start with a ``#`` and location CAN start with ``/``

::

   stock_id,location#tag[,orientations]

Example

::

   :MAT:
   stock_id,location#tag
   #new,A#-new

Moves all new boxes (with the new tag) to A and unset the new tag.
Note that tag parameters will also be added to the "per-line" tag. As
in ``:Moves`` tags are applied positively to boxes successfully moved
and negatively to leftover.

::rST -}
readMovesAndTags :: [Text] -> FilePath -> IO (WH [Box s] s)
readMovesAndTags tags0 = readFromRecordWithPreviousStyle go where
  go style (ForMovesAndTags tag'location orientations) =
    let (tags, locM) = splitTagsAndLocation tag'location
    in processMovesAndTags tags0 (style, tags, locM, orientations)



-- | Read Rearrange Boxes
{- rST::rearrange

Reposition the boxes in a cyclic manner to rearrange them, ensuring that
boxes marked with the tag ``#dead`` are either eliminated or relocated
to the end. This process involves filling the resultant gap by shifting
the necessary boxes.

By default, the reorganization occurs exclusively within groups of
identical content. This emulates the refilling of vacant slots with
boxes of the same content, typically sourced from an alternate shelf,
often situated at the top. The retention of dead boxes facilitates the
identification of the intended purpose of the empty slot.

Moreover, instead of relocating all the boxes, the realignment can be
executed in such a way that only a minimal number of boxes need to be
shifted, allowing some boxes to remain in their original positions.

::

   boxes,actions

``actions`` is a list of box selectors indicating where to shift the box
with.The syntax is as follow

::

    [-%/] [!]action1 '>' [!]action2 '>' ...

Within the actions, box selectors exclusively choose a subset of the
main box selector. They are complete box selectors
(``box-pattern[/shelf-pattern]``), unless the entire action begins with
``/``, in which case, all selectors will be shelf selectors (i.e., boxes
from the full box selector in the selected shelf).

If an action begins with ``!``, boxes that remain in the \*selection\*
will stay in their current positions.

If an action begins with ``%``, boxes will be treated as a whole rather
than by content.

If an action begins with ``-``, ``#dead`` boxes will be deleted.

Example (letters indicate content, lowercase denotes dead boxes):

::

   :RAR:
   selector,actions
   T-Shirt,/#top > #bottom -- Shift each box from the top shelf to the bottom shelf based on color
   --
   --        A2 B3 C2 C3| #top       a1 b1 C3 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --
   T-Shirt,/#top > !#bottom -- Same, but the box on the bottom shelf (B2) remains in place
   --
   --        A2 B3 C2 C3| #top       a1 b1 C3 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --
   T-Shirt,/#top > !#bottom -- B2 and C3 remain in place
   --
   --        A2 B3 C2 C3| #top       a1 b1 c1 C3
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B3 B2 C2
   --
   T-Shirt,%/#top > #bottom -- All boxes shift
   --
   --        A2 B3 C2 C3| #top       C3 a1 b1 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    B2 A2 B3 C2
   --
   T-Shirt,%/#top > !#bottom -- B2 remains in place
   --
   --        A2 B3 C2 C3| #top       C3 a1 b1 c1
   --        -----------+-------- => -----------
   --        a1 b1 B2 c1| #bottom    A2 B2 B3 C2
   --ND:

::rST -}
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
-- | Freeze order  
{- rST::freeze-order

Freeze the order boxes are stored internally accordingly to the order
boxes are selected. Shouldn't change much but might improve performance
or provide with a stable order.

::

   :FreezeOrder:
   selector
   * -- resort everything according to default priority
   :END:

::rST -}
readFreezeOrder :: [Text] -> FilePath -> IO (WH [Box s] s)
readFreezeOrder tags0 = readFromRecordWith go
  where go (Csv.Only style) = do
          boxes <- findBoxByNameAndShelfNames style
          freezeOrder $ map boxId boxes
          case parseTagAndPatterns tags0 [] of
            [] -> return boxes
            ops -> zipWithM (updateBoxTags ops) boxes [1..]


-- | Read Shelf Tags 
{- rST::tag-shelf
Tag the shelves containing selected boxes. Tags can be used to
specify the styling of a shelves. Example

::

   :SHELF_TAGS:
   stock_id,tag
   A,tag -- tag all shelves containing A
   /S,tag -- tag shelves with name S
   /#sep,fg=blue -- set the foreground of all shelves having the `sep` tag
   #new/#top,tag -- tag all 'top' shelves containing a items with the new tag


::rST -}
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
{- rST::transform-tags
| Allow to use POSIX regular expression to subsitute existing tags
  into new ones. Depending on if properties are given or not, the
  behavior will be slightly different. Without properties, each tag
  of the selecting boxes are matched against the pattern. A set of
  new tags is generated by substituing the pattern with the
  substitution string which is then splitted using ``#``. Other tags
  can be removed by generating a *negative* tag (using ``-``). The
  original tag is not deleted but can be done using ``-\0``.
| If properties are given, the transformations will only apply to the
  values of those properties. This should be faster but doesn't allow
  renaming or deleting a tag/property. It is a CSV with the following
  header

::

   stock_id,pat(tern),sub(stitue)

Examples

::

   A,black,blue --> add the blue tag to each box of type A
   ,black,blue#-black --> replace black by blue
   ,black,blue#-\0 --> replace black by blue. (remove black)
   ,^[[:upper]],-\0 --> remove all tags starting with an uppercase

Group (using \`(..)\`) can be use to extract substring

::

   ,(..)-(..),\2:\1 --> add BB:AA from the tag AA-BB

Properties and virtual tags are expanded in the regexp itself.
Example

::

   :TAGS:
   stock_id,tag
   ,shelfname=$shelfname -- set shelfname property using shelfname attribute
   :END:
   :TRANSFORM:
   stock_id,pat,sub
   ,location=.*$[shelfname],unmoved -- detect boxes which haven't changed

In this example, we need to use an intermediate property
``shelfname`` because the name of the shelf can contains ``/`` which
are replaced by ``'`` when the tag is set. For example if object A is
in location ``W/2``, it will have a tag ``location=W'2`` (instead of
``location=W/2``). ``$shelfname`` expands to ``W/2`` whereas the
value of the shelfname propery will be W'2 (``shelfname=W'2``). This
behavior might be fixed and therefore this workaround not necessary
in a future versioin. To detect moves only if the the 3 first letter
of the shelf name have changed :

::

   :TAGS:
   stock_id,tag
   ,shelfname=$shelfname -- set shelfname property using shelfname attribute
   :END:
   :TRANSFORM:
   stock_id,pat,sub
   ,shelfname=(...).*,shortshelf=\1
   ,location=(...).*,shortloc=\1
   ,shortshelf=$[shortloc],unmoved -- uses the value of shortloc property
   :END:
   :TRANSFORM_shortshelf:
   stock_id,pat,sub
   ,A,B -- rename the value of short shelf from A to B
   :E
::rST -}
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
{- rST::stocktake
Describes a set of boxes with their location and eventually
orientation. It is a CSV with the following header

::

   Bay No,Style,QTY,Length,Width,Height,Orientations

Tags provided in the section name, will be applied to the created
boxes. For example, all boxes created in the section
``:Stocktake-tag1-tag2`` will be tagged with ``tag1`` and ``tag2`` If
the ``@throwError`` is given and box doesn't fit in the given shelf.
Instead of moving the box to the error shelf, the planner will stop
and generate an error message.
::rST -}
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
                                  Nothing | ":" `isPrefixOf` posSpec -> return [FCBox box Nothing]
                                          | otherwise -> case parseFillCommands posSpec of 
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
                           

-- | Read orientation rules
{- rST::orientation-rules
 
.. _orientation-rules:

Specifies the boxes configuration within a shelves (if they are
stacked up, on the side, how many etc). Boxes of a given style can be
given different configuration for different shelves by specifing the
shelf in the box selector. This is a CSV with the following header:
``stock_id,orientation`` Orientation must have the following format
``no-diagonal stackin-limitg orientations`` Example:

::

   TSHIRT/#top,^
   TSHIRT,!|=

All T-shirt on top shelves (with the tag ``top``) are up, whereas
T-shirt in other shelves are being laid on the side or the other with
no diagonal allowed.

Orientations
''''''''''''

::

   * -- all 
   % -- default orientations
   ^ -- up
   = -- tilted forward
   > -- tilted right
   | -- tilted forward & right
   ' -- rotated up
   @ -- rotated side

max stacking specification
''''''''''''''''''''''''''

By default, boxes are stacked using only one level of depth. This
way, no boxes hide behind others and so all boxes are visible. To
enable the use of multiple depth and allow boxes to hide each other,
a minimum and max depth can set (before) A maximumn limit for height
and width (actual bay length) can be specified (but no minum). Some
or all of the limit can be specified as follow
``depth | depth x height | lenght x depth x height`` Example

::

   ,1:4 -- allow up to 4 depth level
   ,1: -- use a mininum of 2
   ,4 -- similar to 1:4
   ,4^ -- up to 4 levels, stacking boxes up
   ,1x2 -- max depth 1, max height 2
   ,1x2x3 -- max width 1, max depth 2, max height 3
   ,xx3 -- max height 3

::rST -}
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

  
  
