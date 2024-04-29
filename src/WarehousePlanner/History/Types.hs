module WarehousePlanner.History.Types
( HiSTRef(..)
, newHiSTRef
, readHiSTRef
, writeHiSTRef
, Event(..)
, newEvent
, baseLevel
)
where

import ClassyPrelude
import Control.Monad.ST
import Data.STRef
import Data.List.NonEmpty as NE


data HiSTRef a s = HiSTRef (STRef s (NonEmpty (a s, Event)))

newHiSTRef :: Event -> a s ->  ST s (HiSTRef a s)
newHiSTRef ev a = do
  ref <- newSTRef $ pure (a, ev)
  return $ HiSTRef ref

readHiSTRef :: Event -> HiSTRef a s -> ST s (a s)
readHiSTRef _ (HiSTRef ref) = do
  (a, _e) :| _ <- readSTRef ref
  return a
  
writeHiSTRef :: Event -> HiSTRef a s -> a s -> ST s ()
writeHiSTRef NoHistory (HiSTRef ref) a = writeSTRef ref $ pure (a, NoHistory)
writeHiSTRef ev (HiSTRef ref) a = do
  history <- readSTRef ref
  -- remove inital no history
  let value = case history of
                 (_, NoHistory) :| prev -> (a, ev) :| prev
                 _ -> NE.cons (a, ev) history

  writeSTRef ref value




             
  
data Event = NoHistory
           | Event { evParent :: Maybe Event
                   , evPrevious :: Event
                   , evDescription :: Text
                   , evId :: Int
                   , evLevel :: Int
                   }
     deriving (Eq)
instance Show Event where
  show NoHistory = "NoHistory"
  show Event{..} = unwords
                   [ show evLevel <> "." <> show evId
                   , "^" <> maybe "" (show . WarehousePlanner.History.Types.evId ) evParent 
                   , unpack evDescription
                   ]
                   
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
  else evParent >>= parentWithLevel level 
   
eventId :: Event -> Int
eventId NoHistory = 0
eventId Event{evId} = evId

baseLevel :: Int
baseLevel = 1000
