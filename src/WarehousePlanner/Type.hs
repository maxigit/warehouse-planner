{-# LANGUAGE TupleSections, BangPatterns #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module WarehousePlanner.Type
( module WarehousePlanner.Type
, module WarehousePlanner.History.Types
)where
import ClassyPrelude
import Control.Monad.State
import Diagrams.Prelude(Colour)
import Control.Monad.ST
import Data.Set qualified as Set
import Data.Map qualified as Map 
import System.FilePath.Glob qualified as Glob
-- import Data.List(intercalate)
import Data.Semigroup(Arg(..))
import Data.Text qualified as Text
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import WarehousePlanner.History.Types
import Data.Foldable qualified as F

-- * Types 
data Dimension = Dimension { dLength :: !Double
                           , dWidth  :: !Double
                           , dHeight :: !Double
                           } deriving (Show, Eq, Ord)

invert :: Dimension -> Dimension
invert (Dimension l w h) = Dimension (negate l) (negate w) (negate h)

data Direction = Vertical | Depth | Horizontal deriving (Show, Eq, Ord, Enum)
data Flow = LeftToRight | RightToLeft deriving (Show, Eq, Ord, Enum)

defaultFlow :: Flow
defaultFlow = LeftToRight

-- | How to filter box by number.
-- When selecting boxes, we first limit the number of boxes per content, then shelves then total
-- Filtering by content means only n boxes of the same style content will be selected.
-- This is use full to for example only keep one box of each variations and move them on top
data BoxNumberSelector = BoxNumberSelector
   { nsPerContent :: !(Limit)
   , nsPerShelf :: !(Limit)
   , nsTotal :: !(Limit)
   } deriving (Show, Eq)

-- | How to take slice of a selection
data Limit = Limit 
  { liStart :: !(Maybe Int) -- ^ first box to take, starts a 1
  , liEnd :: !(Maybe Int) -- ^ last box to take
  , liOrderingKey :: ![(OrderingKey, SortingOrder)] -- ^ which tag to use to sort boxes
  , liUseBase :: !Bool -- ^ if true add the base key when sorting
  } deriving (Show, Eq)
  
pattern NoLimit <- Limit Nothing Nothing []  _  where
        NoLimit = Limit Nothing Nothing [] True
  
data OrderingKey = OrdTag Text | OrdAttribute Text
     deriving (Show, Eq)
data SortingOrder = NormalOrder | ReverseOrder
    deriving (Show, Eq)

-- | How something is oriented. It indicates  the direction of
-- the normal of the given face.
data Orientation = Orientation {  top :: !Direction, front :: !Direction } deriving (Eq, Ord)

instance Show Orientation where show = unpack . showOrientation

data Position = Position
              { pOffset :: Dimension
              , pOrientation :: Orientation
              }
     deriving (Eq, Show, Ord)

  

data HowMany = HowMany 
             { perShelf :: !Int
             , perLength :: Int
             , perDepth :: Int
             , perHeight :: Int
             } deriving (Show) --  manual => , Eq, Ord)
             
instance Eq HowMany where a == b = perShelf a == perShelf b
instance Ord HowMany where compare a b = compare (perShelf a) (perShelf b)
         
mkHowMany :: Int -> Int -> Int -> HowMany
mkHowMany l w h = HowMany (l*w*h) l w h


-- | Way boxes are arranged.
-- For Diagonal, Size of the square to repeat
-- in diagonal configuration
-- example
--     - - |
--     - | -
--     | - -
-- 0 (or 1) means no diagonal
data TilingMode = Regular HowMany
                | Diagonal HowMany Int
                | TilingCombo Direction TilingMode TilingMode
                deriving (Eq, Show)
                
instance Ord TilingMode where
         compare a b = case tmTotal b `compare` tmTotal a of
                            EQ -> rank a `compare` (rank b :: Int)
                            c -> c
                       where rank Regular{} = 1 :: Int
                             rank (TilingCombo _ m n) = rank m + rank n
                             rank Diagonal{} = 3
                
showOrientationWithDiag :: Orientation -> TilingMode -> Text
showOrientationWithDiag or tilingMode = 
  case tilingMode of
       (Regular _) -> showOrientation' or
       (Diagonal  _ n ) -> "[" <> showOrientation' (rotateO or) <> Text.replicate (n-1) (showOrientation' or) <> "]"
       (TilingCombo dir m1 m2) -> showOrientationWithDiag or m1 <> showD dir <> showOrientationWithDiag (rotateO or) m2
  where showD dir = case dir of
                      Horizontal -> "→"
                      Vertical -> "↑"
                      Depth -> "⊙"

-- | Possible orientations plus min max depth
data OrientationStrategy  = OrientationStrategy
  { osOrientations :: Orientation
  , osMinDepth :: Int
  , osMaxDepth :: Int
  , osMaxLenght:: Maybe Int
  , osMaxHeight :: Maybe Int
  , osUseDiagonal :: Bool -- ^ see `howManyWithDiagonal`
  } deriving (Show, Eq, Ord)

showOrientationStratety OrientationStrategy{..} =
     (if not osUseDiagonal then "!" else "")
     <> mshow osMaxLenght
     <> "x"
     <> case osMinDepth of 
            d | d > 1 -> tshow d<> ":"
            _ -> ""
       <> tshow osMaxDepth
     <> "x"
     <> mshow osMaxHeight
     <> showOrientation' osOrientations
     where mshow mi = case mi of
                        Just i | i > 1 -> tshow i
                        _ -> ""
-- | Every box belongs to a shelf.
-- Non placed boxes belongs to the special default shelf
newtype BoxId s = BoxId_ (Arg Int (HiSTRef Box s)) deriving (Eq, Ord)
{-# COMPLETE BoxId #-}
pattern BoxId s <- BoxId_ (Arg _ s)

instance Show (BoxId s) where
  show (BoxId_ (Arg i _)) = "#" ++ show i

-- Tag equivalent to a page break in a document
-- Indicate if the given box should start a new row or a new shelf
data BoxBreak = StartNewSlot
              | StartNewSlice -- new row or column according to shelf strategy
              | StartNewShelf deriving (Eq, Show, Ord)
-- | Tags with optionals value
type Tags = Map Text (Set Text)
data Box s = Box { _boxId      :: BoxId s
               , boxShelf :: Maybe (ShelfId s)
               , boxStyle    :: !Text
               , boxContent  :: !Text
               , _boxDim     :: !Dimension
               , boxOffset   :: !Dimension
               , orientation :: !Orientation -- ^ orientation of the box
               , boxBoxOrientations :: [Orientation]  -- ^ allowed orientation
               , boxTags :: !Tags -- ^ tags with optional values
               , boxPriorities :: !(Int, Int, Int ) -- Global, within style, within content , default is 100
               , boxBreak :: !(Maybe BoxBreak)
               } deriving (Eq)
               
instance Show (Box s) where
  show box = unpack (boxStyle box <> "-" <> boxContent box) <> show (boxId box)
data ShelfId s = ShelfId_ (Arg Int (HiSTRef Shelf s))  deriving (Eq, Ord)
{-# COMPLETE ShelfId #-}
pattern ShelfId s <- ShelfId_ (Arg _ s)

instance Show (ShelfId s) where
  show (ShelfId_ (Arg i _)) = "#" ++ show i

instance Ord (Box s) where
  compare a b = compare (_boxId a) (_boxId b)


-- | Shelf have a min and max dimension. This allows shelf to be overloaded
-- or boxes to stick out.
data Shelf s = Shelf { _shelfId  :: !(ShelfId s)
                   , _shelfBoxes :: Seq (BoxId s)
                   , shelfName :: !Text
                   , shelfTag :: !Tags
                   , minDim    :: !Dimension
                   , maxDim    :: !Dimension
                   , flow      :: !Flow
                   , shelfBoxOrientator :: !BoxOrientator
                   , shelfFillingStrategy :: !FillingStrategy
                   , bottomOffset :: !Double -- ^ "altitute" of where the usable part of the shelf starts
                   } deriving (Show, Eq)

instance Ord (Shelf s) where
  compare a b = compare (_shelfId a) (_shelfId b)

-- | Gives orientation to a box
data BoxOrientator = DefaultOrientation
    | ForceOrientations ![Orientation]
    | BoxOrientations
    | FilterOrientations ![Orientation]
    | AddOrientations ![Orientation] ![Orientation]
    deriving (Show, Eq, Ord)


-- |
newtype Shelfname = Shelfname Text deriving Show
-- | Which way fill shelves

data FillingStrategy = RowFirst | ColumnFirst deriving (Show, Eq, Enum, Ord)
type RunsWithId s = Runs NonEmpty (ShelfId s)


-- | State containing bays of boxes
-- boxOrientations : function returning a list
-- of possible box orientiations within a shelf for a given box.
-- 
data Warehouse s = Warehouse { boxMap :: Map Text  (Seq (BoxId s))
                           , shelves :: Seq (ShelfId s)
                           , shelfGroup :: RunsWithId s
                           , boxStyling :: Box s -> BoxStyling
                           , shelfStyling :: Shelf s -> ShelfStyling
                           , boxOrientations :: Box s -> Shelf s -> [OrientationStrategy]
                           , whCacheM :: Maybe (STRef s (OperationCache s))
                           , whDay :: Day -- Today usefull to compute date operation
                           -- \^ a cache. We use maybe to that an empty warehouse can be created "purely"
                           -- Should probably be part of of the WH 
                           , whUnique :: Int
                           , whCurrentEvent :: Event
                           , whEventHistory ::  [Event] -- not including current
                           
             } -- deriving Show
             
boxes :: Warehouse s -> Seq (BoxId s)
boxes warehouse = mconcat $ map snd $ Map.toAscList $ boxMap  warehouse

type WH a s = StateT  (Warehouse s) (ST s) a
data ExitMode = ExitOnTop | ExitLeft deriving (Show, Eq, Ord, Enum)
-- | Strategry to find available rectangle to fill in
data PartitionMode
  = PAboveOnly -- ^ max used height only
  | PRightOnly -- ^ max used weight only
  | PBestEffort -- ^ try to find all available rectangles
  | PCorner Int -- ^ specify the exact corner in best effort
  | POverlap OJustify -- ^ partition as if shelf was empty then removed used slots.
  | PSortedOverlap -- ^ partition as if shelf was empty then removed used slots. but keep overall boxe in order
  | PBehind
  | POr PartitionMode PartitionMode -- ^ combination
  deriving (Show, Eq, Ord)
  
-- | Overlap justify mode
-- Overlapping works by juxtaposing a grid (as if the shelf was empty)
-- and remove the position which are not free (Because a box is overlapping it).
-- For best result, it might be necessary to adjust the grid 
-- To the right (ORight) or so that one of the line vertical line
-- of the grid is aligned with the last of the used box.
--  
data OJustify = OLeft 
              | ORight
              | OAligned
     deriving (Show, Eq, Ord)

data AddOldBoxes = NewBoxesOnly | AddOldBoxes deriving (Show, Eq, Ord, Enum)


-- | Misc data to speed up warehouse operations
-- depending on the value, it should be setup by the caller or the callee
data OperationCache s  = OperationCache
  { propertyStats :: Map Text PropertyStats
  , boxTagMapMap :: Map Text (Map Text [Box s])
  }
emptyOperationCache :: OperationCache s
emptyOperationCache = OperationCache mempty mempty

-- | Statistics relative to a property 
-- used valued and number of values
-- this is used to convert a property value to its rank or index
-- The rank can be used to chose a colour in a color scale.
data PropertyStats = PropertyStats
   { totalCount :: Int
   , valueRank :: Map Text Int -- sorted by occurence
   , valueIndex :: Map Text Int -- sorted alpha
   } deriving Show

type Corner = (Double, Double)

-- | how to render boxes
data BoxStyling = BoxStyling
  { foreground :: Colour Double --  ^ Text colour
  , background :: Colour Double -- ^ Background colour
  , circleBgs :: [Colour Double] -- ^ 2nd Background colour
  , border :: Maybe (Colour Double)  -- ^ border colour if different from foreground
  , title :: [ Text ] -- ^ text to display 
  , barTitle :: Maybe Text -- ^ text to display in the bar
  , displayBarGauge :: Bool -- ^ to display or the bar gauge
  , offsetBarGaugeX :: Maybe Double
  , offsetBarGaugeY :: Maybe Double
  } deriving (Show, Eq)
  
data ShelfStyling = ShelfStyling
  { foreground :: Colour Double --  ^ Text colour
  , background :: Colour Double -- ^ Background colour
  , maxBackground :: Colour Double -- ^ Background colour
  , barForeground :: Colour Double -- ^ Text colour
  , barBackground :: Colour Double -- ^ Background colour
  , border :: (Colour Double)  -- ^ border colour 
  , title :: [ Text ] -- ^ text to display 
  , barTitle :: Maybe Text -- ^ text to display in the bar
  , displayBarGauge :: Bool -- ^ to display or the bar gauge
  } deriving (Show, Eq)

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

-- * Classes 
class ShelfIdable a where
    shelfId :: a s -> ShelfId s
class (ShelfIdable b) => Shelf' b where
  findShelf :: b s  -> WH (Shelf s) s
class BoxIdable a where
    boxId :: a s -> BoxId s
class (BoxIdable b) => Box' b where
  findBox :: b s  -> WH (Box s) s
class Referable a where
  type Ref a :: *
  getRef :: a -> Ref a

class HasTags a where
  getTags :: a -> Tags

-- * Instances 
instance Semigroup Dimension where
    (Dimension l w h) <> (Dimension l' w' h') =
            Dimension (l+l') (w+w') (h+h')
instance Monoid Dimension where
    mempty = Dimension 0 0 0

instance BoxIdable BoxId where
    boxId b = b

instance BoxIdable Box where
    boxId = _boxId

instance Box' Box where

  findBox b = findBox (boxId b) -- "reload" the box in caes it has been modified
instance Box' BoxId where
  findBox (BoxId ref) = do
          ev <- gets whCurrentEvent 
          lift $ readHiSTRef ev ref


instance Referable (Box s) where
  type Ref (Box s) = HiSTRef Box s
  getRef box = getRef (boxId box)

instance Referable (BoxId s) where
  type Ref (BoxId s) = HiSTRef Box s
  getRef (BoxId ref) = ref

instance Referable (Shelf s) where
  type Ref (Shelf s) = HiSTRef Shelf s
  getRef shelf = getRef (shelfId shelf)

instance Referable (ShelfId s) where
  type Ref (ShelfId s) = HiSTRef Shelf s
  getRef (ShelfId ref) = ref

instance ShelfIdable ShelfId where
    shelfId b = b

instance ShelfIdable Shelf where
    shelfId = _shelfId
instance Shelf' Shelf where
  findShelf s = findShelf (shelfId s) -- reload the shef
instance Shelf' ShelfId where
  findShelf (ShelfId ref) = do
            ev <- gets whCurrentEvent
            lift $ readHiSTRef ev ref

instance HasTags (Box s) where getTags = boxTags
instance HasTags (Shelf s) where getTags = shelfTag
instance HasTags (Tags) where getTags = id

-- * Utilities 
-- ** Dimensions 
volume :: Dimension -> Double
volume (Dimension l w h) = l*w*h

floorSpace :: Dimension -> Double
floorSpace (Dimension l w _) = l*w

maxDimension :: [Dimension] -> Dimension
maxDimension dims = foldl' overlap mempty dims where
             overlap (Dimension l w h) (Dimension l' w' h') = Dimension (max l l') (max w w') (max h h')

minDimension :: [Dimension] -> Dimension
minDimension [] = mempty
minDimension (dim:dims) = foldl' overlap dim dims where
             overlap (Dimension l w h) (Dimension l' w' h') = Dimension (min l l') (min w w') (min h h')

dimensionToList :: Dimension -> [Double]
dimensionToList (Dimension l w h) = [l, w, h]

dimensionSame :: Dimension -> Dimension -> Bool
dimensionSame d1 d2 = sort (dimensionToList d1) == sort (dimensionToList d2)
-- ** Tiling
tmTotal :: TilingMode -> Int
tmTotal (Regular hmany) = perShelf hmany
tmTotal (Diagonal hmany _) = perShelf hmany
tmTotal (TilingCombo _ t1 t2) = tmTotal t1 + tmTotal t2

tmLength :: TilingMode -> Int
tmLength (Regular hmany) = perLength hmany
tmLength (Diagonal hmany _) = perLength hmany
tmLength (TilingCombo Horizontal t1 t2) = tmLength t1 + tmLength t2
tmLength (TilingCombo _ t1 t2) = tmLength t1 `max` tmLength t2

tmDepth :: TilingMode -> Int
tmDepth (Regular hmany) = perDepth hmany
tmDepth (Diagonal hmany _) = perDepth hmany
tmDepth (TilingCombo Depth t1 t2) = tmDepth t1 + tmDepth t2
tmDepth (TilingCombo _ t1 t2) = tmDepth t1 `max` tmDepth t2

tmHeight :: TilingMode -> Int
tmHeight (Regular hmany) = perHeight hmany
tmHeight (Diagonal hmany _) = perHeight hmany
tmHeight (TilingCombo Vertical t1 t2) = tmHeight t1 + tmHeight t2
tmHeight (TilingCombo _ t1 t2) = tmHeight t1 `max` tmHeight t2

tmHowManys :: TilingMode -> NonNull [HowMany]
tmHowManys (Regular hmany) = ncons hmany []
tmHowManys (Diagonal hmany _) = ncons hmany []
tmHowManys (TilingCombo _ m1 m2) = tmHowManys m1 <> tmHowManys m2

-- ** Boxes 

-- ** Orientations 
up, tiltedForward, tiltedRight, tiltedFR, rotatedSide, rotatedUp :: Orientation
up = Orientation Vertical Depth
tiltedForward = Orientation Depth Vertical
tiltedRight = Orientation Horizontal Depth
tiltedFR = Orientation Depth Horizontal
rotatedSide = Orientation Horizontal Vertical
rotatedUp = Orientation Vertical Horizontal

showOrientation :: Orientation -> Text
showOrientation o = showOrientation' o <> " "
showOrientation' :: Orientation -> Text
showOrientation' o | o == up             =  "^"
                  | o == tiltedForward  =  "="
                  | o == tiltedRight    =  ">"
                  | o == tiltedFR       =  "|"
                  | o == rotatedUp      =  "'"
                  | o == rotatedSide      =  "@"
                  | otherwise           =  "tA"

readOrientation :: Char -> Orientation
readOrientation c = fromMaybe (error ("can't parse orientation '" <> show c )) $ readOrientationMaybe c

readOrientationMaybe :: Char -> Maybe Orientation
readOrientationMaybe c = case c of
    '^' -> Just up
    '=' -> Just tiltedForward
    '>' -> Just tiltedRight
    '|' -> Just tiltedFR
    '\'' -> Just rotatedUp
    '@' -> Just rotatedSide
    _ -> Nothing 

allOrientations :: [Orientation]
allOrientations = [ up
                  , rotatedUp
                  , tiltedForward
                  , tiltedFR
                  , tiltedRight
                  , rotatedSide
                  , rotatedUp
                  ]

rotate :: Orientation -> Dimension -> Dimension
rotate o (Dimension l w h)
    | o == up             =  Dimension l w h
    | o == tiltedForward  =  Dimension l h w
    | o == tiltedRight    =  Dimension h w l
    | o == tiltedFR       =  Dimension w h l
    | o == rotatedUp    =  Dimension w l h
    | o == rotatedSide    =  Dimension h l w
    | True  = error $ "Unexpected rotation" <> show o

-- | Rotate an Orientation by 90 facing the depth
rotateO :: Orientation -> Orientation
rotateO (Orientation{..}) = Orientation (rot top) (rot front)
  where 
    rot Vertical = Horizontal
    rot Horizontal = Vertical
    rot Depth = Depth


-- ** Boxes 
boxKey :: Box s -> Text
boxKey b = (boxStyle b) <> (boxContent b)
boxSku :: Box s -> Text
boxSku b = boxStyle b <> "-" <> boxContent b
getTagList :: HasTags tagged => tagged -> [Text]
getTagList = map flattenTag'Values . Map.toList . getTags
-- | convenience function transforming  the result to list
-- Can be easier to pattern match
getTagValues :: HasTags tagged => tagged -> Text -> [ Text ]
getTagValues box tag = maybe [] (Set.toList) (Map.lookup tag (getTags box))
getTagValuem :: HasTags tagged => tagged -> Text -> Maybe Text
getTagValuem box tag = fmap flattenTagValues (Map.lookup tag (getTags box))
-- | Like getTagValuesWithPresence but return [""] if a tag is set without value
getTagValuesWithPresence :: HasTags tagged => tagged -> Text -> [Text]
getTagValuesWithPresence box tag =
  case getTagValues box tag of
    [] | tagIsPresent box tag -> [""]
    values -> values
tagIsPresent :: HasTags tagged => tagged -> Text -> Bool
tagIsPresent box tag = Map.member tag (getTags box)

-- | Return global dimension according
-- to box orientation
boxDim :: Box s -> Dimension
boxDim box = rotate (orientation box) (_boxDim box)


boxVolume :: Box s -> Double
boxVolume = volume . _boxDim

-- | Returns box offset <> box itself
boxCorner :: Box s -> Dimension
boxCorner b = boxOffset b <> boxDim b


-- ** Tags 
flattenTagValues :: Set Text -> Text
flattenTagValues = intercalate ";" . Set.toList
flattenTag'Values :: (Text, Set Text) -> Text
flattenTag'Values (tag, values) = case flattenTagValues values of
                      "" ->tag
                      flat -> tag <> "=" <> flat

flattenTags :: Tags -> [Text]
flattenTags = map flattenTag'Values . Map.toList 
-- ** Shelves 
shelfVolume :: Shelf s -> Double
shelfVolume = volume . minDim

shelfNameTag :: Shelf s -> Text
shelfNameTag s = intercalate "#" ( shelfName s
                                 : (flattenTags (shelfTag s))
                                 )
-- ** Selectors 
-- | Some cases are overlapping but it is on purpose
-- so that we can speed up 
data TagSelector  a
          = TagHasKey !MatchPattern --  ^ check present of key not is value
          | TagHasNotKey !MatchPattern --  ^ check present of key not is value
          | TagIsKey !MatchPattern --  ^ present with no value
          -- -| TagHasNotKey !MatchPattern --  ^ not present  (in all values)
          -- -| TagIsKeyAndValue Text Text --  ^ check key and value
          | TagIsKeyAndValues !MatchPattern ![ValuePattern] --  ^ check key and all values are exact
          | TagHasKeyAndValues !MatchPattern ![ValuePattern] --  ^ check key and at least all values are present
          | TagHasValues ![ValuePattern]
          | TagHasNotValues ![ValuePattern]
          | TagHasKeyAndNotValues !MatchPattern ![ValuePattern] --  ^ check key and at least all values are present
          deriving (Show, Eq)
          -- deriving (Eq,Show,Ord)

data MatchPattern
   = MatchFull !Text
   | MatchAnything
   | MatchOrd Ordering Bool Text
   | MatchGlob !Glob.Pattern
   deriving (Eq, Show)
   -- -| MatchRegext Text

-- | Pattern or tag reference
data ValuePattern 
   = VMatch MatchPattern
   | VTag Text
   deriving (Eq, Show)
   

 

data NameSelector a = NameMatches [MatchPattern] -- matches one of
                    | NameDoesNotMatch [MatchPattern] -- matche none of
  deriving (Eq, Show)

data Selector a  = Selector 
  { nameSelector :: !(NameSelector a)   -- disjunctions OR betweens names
  , tagSelectors :: ![TagSelector a] -- conjunctions AND
  }
  deriving (Show, Eq)

pattern SelectAnything :: Selector a
pattern SelectAnything = Selector AnyNames []


matchAnyNames :: NameSelector a -> Bool
matchAnyNames (NameMatches []) = True
matchAnyNames (NameMatches [MatchAnything]) = True
matchAnyNames (NameDoesNotMatch []) = True
matchAnyNames _ = False

pattern AnyNames :: NameSelector a
pattern AnyNames <-  (matchAnyNames  -> True) where
        AnyNames = NameMatches [MatchAnything]


data BoxSelector = BoxSelector
  { boxSelectors :: !(Selector Box)
  , shelfSelectors :: !(Selector Shelf)
  , numberSelector :: !(BoxNumberSelector)
  } deriving (Show, Eq)

data ShelfSelector = ShelfSelector
  { sBoxSelectors :: !(Selector Box)
  , sShelfSelectors :: !(Selector Shelf)
  } deriving (Show, Eq)

selectAllBoxes :: BoxSelector
selectAllBoxes = SelectAllBoxes
pattern SelectAllBoxes = BoxSelector SelectAnything
                             SelectAnything
                             (BoxNumberSelector NoLimit NoLimit NoLimit)


selectAllShelves :: ShelfSelector
selectAllShelves = SelectAllShelves
pattern SelectAllShelves = ShelfSelector SelectAnything SelectAnything
-- ** For moves
data SortBoxes = SortBoxes | DontSortBoxes
     deriving (Eq, Ord, Show)
-- ** For Context
-- | Pair of included and excluded things.
-- Used to keep track of which boxes have been processed or not
data InExcluded a = InExcluded 
                  { included :: Maybe [a]
                  , excluded :: Maybe [a]
                  }
     deriving (Eq, Show, Functor, Foldable, Traversable)
pattern AllOf, NoneOf :: Maybe [a ]
pattern AllOf = Nothing
pattern NoneOf = Just []

instance Ord a => Semigroup (InExcluded a) where
   -- included a | included b
   -- exclude a & included b
   -- Keep the original order
   a <> b = let ins = case (included a, included b) of
                          (AllOf, _) -> AllOf
                          (_, AllOf) -> AllOf
                          (NoneOf, is) -> is
                          (is, NoneOf) -> is
                          (Just as, Just bs) -> Just $ as <> filter (flip Set.notMember (Set.fromList as)) bs
                          _ -> error "should be exhaustive"
                exs = case (excluded a, excluded b) of
                          (AllOf, es) -> es
                          (es, AllOf) -> es
                          -- (NoneOf, _) -> NoneOf -- covered by Just []
                          -- (_, NoneOf) -> NoneOf
                          (Just as, Just bs) -> Just $ as <> filter (flip Set.notMember (Set.fromList as)) bs
                          _ -> error "should be exhaustive"
            in InExcluded ins exs
                          
instance Ord a => Monoid (InExcluded a) where
    mempty = InExcluded NoneOf AllOf
allIncluded :: InExcluded a
allIncluded = InExcluded AllOf NoneOf

inverseInEx :: InExcluded a -> InExcluded a
inverseInEx ie = InExcluded (excluded ie) (included ie)

includedList :: InExcluded a -> [a]
includedList = fromMaybe [] . included
excludedList :: InExcluded a -> [a]
excludedList = fromMaybe [] . excluded


narrowIncluded :: Show a => Ord a => (a -> Bool) -> InExcluded a -> InExcluded a
narrowIncluded keep inEx = let
   (ins, outs) = partition keep (includedList inEx)
   toExclude = inEx <> (InExcluded Nothing (Just outs))
   in -- traceShow ("NARROW", as) 
      -- $ traceShow ("IN", included inEx) 
      -- $ traceShow ("EX", excluded inEx) 
      -- $ traceShow ("ins", ins)
      -- $ traceShow ("out", outs)
      -- $ traceShow ("<>", toExclude ) 
      -- $ traceShowId
     --  $
      InExcluded (Just ins) (excluded toExclude)
excludeIncluded :: (Ord k, Ord a) => (a -> k) -> [a] -> InExcluded a -> InExcluded a
-- excludeIncluded _ outs (InExcluded AllOf Nothing) = InExcluded AllOf (Just outs)
excludeIncluded key outs inEx = let
  excludeSet = Set.fromList $ map key outs
  keep = (`notMember` excludeSet) . key
  ins  = case included inEx of 
            AllOf -> AllOf
            _ -> Just $ filter keep (includedList inEx)
  toExclude = inEx <> (InExcluded Nothing (Just outs))
  in InExcluded (ins) (excluded toExclude)
   


-- | Remove givin items from both included and excluded
-- Needed when a box is deleted for example.
-- Even thought in theory it should be excluded, it shouldn't.
excludeInEx  :: (Ord k, Ord a) => (a -> k) -> [k] -> InExcluded a -> InExcluded a
excludeInEx key outs inEx = let
  excludeSet = Set.fromList $ outs
  keep = (`notMember` excludeSet) . key
  ins  = case included inEx of 
            AllOf -> AllOf
            _ -> Just $ filter keep (includedList inEx)
  exs = case excluded inEx of
            AllOf -> AllOf
            _ -> Just $ filter keep (excludedList inEx)
  in InExcluded ins exs
  

  
   
-- as well as the number of boxes which can be used for the depth (min and max)
-- setting min to 1, allow forcing boxes stick out
--
-- ** Warehouse 

type Run f a = f (Bay f a)
type Bay f a = f a
type Runs f a = f (Run f a)

  
fromRuns3 :: (Functor f) => (f (Run g b) -> Runs g b) -> (f (Bay g b) -> Run g b)  -> (f a -> Bay g b) -> Runs f a -> Runs g b
fromRuns3 f1 f2 f3 runs = f1 $ fmap fromRun runs where
  fromRun bays = f2 $ fmap fromBay bays 
  fromBay shelves = f3 shelves 

fromRuns :: Functor f => (forall x . f x -> g x) -> Runs f a -> Runs g a
fromRuns f = fromRuns3 f f f

mapRuns :: Functor f => (a -> b) -> Runs f a -> Runs f b
mapRuns f = fmap  (fmap (fmap f) )

traverseRuns :: (Monad m, Traversable f) => (a -> m b) -> Runs f a -> m (Runs f b)
traverseRuns f = traverse (traverse (traverse f))

toRunsL :: (Functor f , Foldable f) => Runs f a -> Runs [] a
toRunsL = fromRuns F.toList

toBayE :: Foldable f => Bay f a -> Maybe (Bay NonEmpty a)
toBayE = nonEmpty . F.toList

toRunE :: Foldable f => Run f a -> Maybe (Run NonEmpty a)
toRunE bays = let
   baysE = mapMaybe toBayE $ F.toList bays
   in nonEmpty baysE
   
toRunsE :: Foldable f => Runs f a -> Maybe (Runs NonEmpty a)
toRunsE  runs = let
   runsE = mapMaybe toRunE $ F.toList runs
   in nonEmpty  runsE
   
filterRuns :: (Functor f, Foldable f) => (a -> Bool) -> Runs f a -> Maybe (Runs NonEmpty a)
filterRuns keep runs = toRunsE 
                     $ fmap (fmap $ filter keep)
                     $ toRunsL runs 

-- ** Tag
