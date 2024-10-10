module WarehousePlanner.Check
( tagBoxesStatus )
where

import ClassyPrelude
import WarehousePlanner.Base
import WarehousePlanner.Affine
import Data.Map qualified as Map
import Text.Printf (printf)


data StickoutStatus = InShelf -- ^ box totally in shelf, no problem
                    | InMaxShelf -- ^ box within max dimension but stick out of min Dimension 
                    | OutOfMaxShelf
     deriving (Eq, Ord, Show)
     
     
     
isBoxStickingOut :: Box s -> Shelf s -> StickoutStatus
isBoxStickingOut box shelf =
    if | affDimensionContains (fromShelf minDim) boxAff -> InShelf
       | affDimensionContains (fromShelf maxDim) boxAff -> InMaxShelf
       | otherwise -> OutOfMaxShelf 
    where boxAff = boxAffDimension box
          fromShelf d = AffDimension mempty (d shelf)
          
-- | Computes the list of boxes overlapping with the given one
boxOverlappWith :: Box s -> [Box s] -> [Box s]
boxOverlappWith box boxes = filter (affDimensionOverlap (boxAffDimension box) . boxAffDimension) boxes
  


-- | Return box status for all boxes of a shelf
checkShelfStatus :: Shelf s -> WH [ (Box s, (StickoutStatus, [Box s])) ] s
checkShelfStatus shelf = do
  boxes <- findBoxByShelf shelf
  let   go box = (box, (isBoxStickingOut box shelf, boxOverlappWith box $ filter (/=box) boxes))
  return $ map go $ reverse boxes
                
-- | Tag all boxes with their check status (shelf + overlaps) if needed
tagBoxesStatus :: Shelf s -> WH () s
tagBoxesStatus shelf = do
  status <- checkShelfStatus shelf
  mapM_ (uncurry tagBoxStatus) status


tagBoxStatus :: Box s -> (StickoutStatus, [Box s]) -> WH () s
tagBoxStatus box (status, overlappings) = do
  let tagOps = [ ("@stickout",  case status of
                                     OutOfMaxShelf -> SetValues []
                                     _ -> RemoveTag
                 )
               , ("@overhang", case status of
                                    InMaxShelf -> SetValues []
                                    _ -> RemoveTag
                 )
               ] <> [ (tag, RemoveTag)
                    | tag <- Map.keys (boxTags box)
                    , "@ogroup-" `isPrefixOf` tag
                    ]
               <> case overlappings of 
                       [] -> map (,RemoveTag) ["@overlap", "@ogroup", "@ogroup-leader", "@overlapping", "@ovolume", "@ovol"]
                       _ -> let ids@(base:_) = map (tshow . boxId) allboxes
                                allboxes = sort (box : overlappings)
                                box'dims = [ (b, dimM )
                                           | b <- allboxes
                                           , let dimM = fmap affineToDimension $ affDimensionIntersection (boxAffDimension box) (boxAffDimension b)
                                           ]
                                vol = maximumEx [ maybe 0 volume dimM
                                                    | (b, dimM) <- box'dims
                                                    , b /= box
                                                    ]
                            in ("@overlap", SetValues []) 
                               : ("@ogroup-base", SetValues [base])
                               : ("@ogroup", SetValues ids)
                               -- includes current box so that the group is the same for all boxes in the same group
                               : ("@overlapping", SetValues $ map (\b -> tshow b <> boxPositionSpec  b) overlappings)
                               : ("@ovolume", SetValues [pack $ printf "%09f" vol ])
                               : ("@ovol", SetValues [pack $ printf "%02.0f%%" (100 * (vol / boxVolume box)) ])
                               : [ ("@ogroup-" <> drop 1 (tshow bid)
                                   , SetValues $ maybe [] (pure . printDim) dimM
                                   )
                                 | (b, dimM) <- box'dims
                                 , let bid = boxId b
                                 ]
               <>
               [ ("@check", SetValues [tshow status ])
               ]
  void $ updateBoxTags tagOps box 0

   
