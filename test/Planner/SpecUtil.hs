module Planner.SpecUtil
( makeShelves
, makeBoxes
, boxesWithId
) where
{-# LANGUAGE NOINLINE makeShelves, makeShelves #-}

import ClassyPrelude
import WarehousePlanner.Base
import WarehousePlanner.Selector(parseSelector, parseBoxNumberSelector, parseBoxSelector)
import WarehousePlanner.Report (boxStyleWithTags)
import Data.Text (breakOn)

-- | Create shelves from a list on name.
-- ex: "S1 S2" creates two shelves S1 and S2
makeShelves :: [Text] -> WH [Shelf s] s
makeShelves = mapM go where
    go name0 = let (name, tags) = breakOn "#" name0
                   tagm = case tags of
                           "" -> Nothing
                           _ -> Just $ drop 1 tags
              in newShelf name tagm shelfDim shelfDim 0 DefaultOrientation ColumnFirst
    shelfDim = Dimension 300 80 150
    
    
-- | Create boxes from a list of strings
-- shelfname boxnames.
-- ex.   "S1 A-1 A-2"
--       "S2 B#2"
--       "S1 C-1"
-- create boxes A-1 A-2 B & C1 in the corresponding shelves (S1 and S2)
makeBoxes :: [Text] -> WH [(Box s, Shelf s)] s
makeBoxes shelfboxess = do
   boxess <- forM shelfboxess  \shelf'boxes  -> do
                  let (shelf:boxes) = words shelf'boxes
                  mapM (go shelf) boxes
                 
   return $ concat boxess
   where go shelfname name = do
            [shelfId] <- findShelfBySelector (parseSelector shelfname)
            shelf <- findShelf shelfId
            let (style', tags) = extractTags name
                (style, drop 1 -> content) = breakOn "-" style'
            box <- newBox style content dim  up shelf [up] tags
            return (box, shelf)
         dim = Dimension 50 30 40

  -- *  Display with # value of tag #id.
  -- Example B-1#id=3#fg=black => B-1#3
boxesWithId :: [Box s] -> Text
boxesWithId = unwords . map ((\b -> boxStyleAndContent b <> maybe "" ("#" <>) (getTagValuem b "id"))) 
