module WarehousePlanner.History.Types
( HiSTRef(..)
, newHiSTRef
, readHiSTRef, readHiSTRef'
, writeHiSTRef
, Event(..), evPreviousM, evParent
, newEvent
, baseLevel
, History
, HistoryRange(..)
, fromHistory
, displayEvent
, DiffStatus(..)
, ZHistory(..), ZHistory1
, zCurrentWithEvent, zCurrent
, zCurrentEx
, zAt, zAtWithEvent
)

where

import ClassyPrelude
import Control.Monad.ST
import Data.STRef
import Data.List.NonEmpty as NE
import Data.Map qualified as Map


type History a s = NonEmpty (Event, a s)
data HiSTRef a s = HiSTRef (STRef s (History a s))

fromHistory :: History a s -> a s
fromHistory = snd . NE.head

newHiSTRef :: Event -> a s ->  ST s (HiSTRef a s)
newHiSTRef ev a = do
  ref <- newSTRef $ pure (ev, a)
  return $ HiSTRef ref

readHiSTRef :: Show (a s) => Event -> HiSTRef a s -> ST s (a s)
readHiSTRef ev ref = fmap snd $ readHiSTRef' ev ref

readHiSTRef' :: Show (a s) => Event -> HiSTRef a s -> ST s (Event, a s)
readHiSTRef' ev (HiSTRef ref) = do
  history <- readSTRef ref
  -- find first element with event <= given event
  let 
  case NE.dropWhile ((> ev) . fst) history of
     [] -> return $ NE.last history --  error $ "Finding reference in the past for " ++ show ev ++ " in " <> show history
     -- ^ should not normally happen as reference in the past should not be given.
     history :_ -> return history
  
writeHiSTRef :: Event -> HiSTRef a s -> a s -> ST s ()
writeHiSTRef NoHistory (HiSTRef ref) a = writeSTRef ref $ pure (NoHistory, a)
writeHiSTRef ev (HiSTRef ref) a = do
  history <- readSTRef ref
  -- override if same event
  let value = case history of
                 (NoHistory, _) :| _ -> (ev, a) :| []
                 (e, _) :| prev | e == ev -> (ev, a) :| prev
                 _ -> NE.cons (ev, a) history

  writeSTRef ref value




             
  
data Event = NoHistory
           | Event { evParent_ :: Maybe Event
                   , evPrevious :: Event
                   , evDescription :: Text
                   , evId :: Int -- ^ Must be unique
                   , evLevel :: Int
                   }
instance Show Event where
  show NoHistory = "#@"
  show Event{..} = "#" <> show evId <> level where
       level = if | evLevel < 10        -> "/" <> show evLevel
                  | evLevel < baseLevel -> "/*"
                  | otherwise           -> ""

evPreviousM :: Event -> Maybe Event
evPreviousM NoHistory = Nothing
evPreviousM Event{evPrevious} = Just evPrevious

evParent :: Event -> Maybe Event
evParent NoHistory = Nothing
evParent Event{evParent_} = evParent_

 
displayEvent :: Event -> Text
displayEvent NoHistory = "NoHistory"
displayEvent ev@Event{..} = unwords
                [ pack (show ev)
                -- , "^" <> maybe "" tshow  evParent 
                , evDescription
                ]

instance Eq Event where
   NoHistory == NoHistory = True
   NoHistory == _  = False
   _ == NoHistory  = False
   e1 == e2 = evId e1 == evId e2

instance Ord Event where
  compare NoHistory NoHistory = EQ
  compare NoHistory _ = LT
  compare _ NoHistory = GT
  compare e1 e2 = compare (evId e1) (evId e2)
                   
-- | Create a new event at the given level
newEvent :: Int -> Event -> Text -> Event 
newEvent level NoHistory description = Event Nothing NoHistory description 1 level
newEvent level previous description = let
    parent = Just previous >>= parentWithLevel (level - 1)
    -- reset id on level 0 (new file)
    -- so that even correspond to line number
    eId = if False -- level == 0
          then 1
          else eventId previous + 1
    in Event parent previous description eId level

-- | Find the parent corresponding to the given level
parentWithLevel :: Int -> Event -> Maybe Event
parentWithLevel _ NoHistory = Nothing
parentWithLevel level e@Event{..} = 
  if evLevel <= level 
  then Just e
  else evParent_ >>= parentWithLevel level 
   
eventId :: Event -> Int
eventId NoHistory = 0
eventId Event{evId} = evId

baseLevel :: Int
baseLevel = 1000


-- * Diff
data DiffStatus s = DiffStatus 
     { dsBoxUpdated :: Int
     , dsBoxDeleted :: Int
     , dsBoxCreated :: Int
     , dsBoxOut :: s
     , dsBoxIn :: s
     , dsBoxShuffled :: Int
     }
     deriving (Show, Eq)

instance Semigroup s => Semigroup (DiffStatus s) where
   d1 <> d2 = DiffStatus (dsBoxUpdated d1 + dsBoxUpdated d2)
                         (dsBoxDeleted d1 + dsBoxDeleted d2)
                         (dsBoxCreated d1 + dsBoxCreated d2)
                         (dsBoxOut d1 <> dsBoxOut d2)
                         (dsBoxIn d1 <> dsBoxIn d2)
                         (dsBoxShuffled d1 + dsBoxShuffled d2)

instance Monoid s => Monoid (DiffStatus s) where
  mempty = DiffStatus 0 0 0 mempty mempty 0
     
     

data HistoryRange = HistoryRange
                  { hrCurrent :: Event -- ^ the event being currently displayed
                  , hrToDiff :: Event -- ^ the event to show diff with
                  }
     deriving (Show, Eq)
     
-- *  Zipper

data ZHistory a = ZHistory { zBefore :: Map Event a
                         , zAfter :: Map Event a
                         }
     deriving (Show)
zCurrentWithEvent :: ZHistory a -> Maybe (Event, a)
zCurrentWithEvent =  Map.lookupMax . zBefore

zCurrent :: ZHistory a -> Maybe a
zCurrent = fmap snd . zCurrentWithEvent

zCurrentEx :: ZHistory a -> a
zCurrentEx z@ZHistory{..} = case zCurrent z of
               Just x -> x
               Nothing -> case Map.lookupMin  zAfter of
                             Just x -> snd x
                             Nothing -> error "The unexpected happen."
zAtWithEvent :: Event -> ZHistory a -> Maybe (Event, a)
zAtWithEvent ev ZHistory{..} = Map.lookupLE ev zBefore <|> Map.lookupGE ev zAfter
zAt :: Event -> ZHistory a -> Maybe a
zAt ev zh = fmap snd $ zAtWithEvent ev zh

type ZHistory1 a f = ZHistory (a f)
