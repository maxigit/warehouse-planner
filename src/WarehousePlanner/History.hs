module WarehousePlanner.History 
( HiSTRef
, newHiSTRef
, readHiSTRef
, writeHiSTRef
, Event(..)
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




             
  
data Event = NoHistory | Event
   
