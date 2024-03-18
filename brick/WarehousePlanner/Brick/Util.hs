module WarehousePlanner.Brick.Util
(
eigthH, eigthV
, percUsed
, percToLevel
, percToAttrName
, generateLevelAttrs
, defaultStyleAttrs
, makeStyleAttrName
, succ', pred'
, styleNameWithAttr
, withStyleAttr
) where

import ClassyPrelude hiding (on)
import WarehousePlanner.Base
import Brick
import Graphics.Vty.Attributes qualified as V

percUsed :: [Shelf s] -> WH Double s
percUsed shelves = do
  boxess <- mapM findBoxByShelf shelves
  let boxesV = sum $ map boxVolume $ concat boxess
      shelvesV = sum $ map shelfVolume shelves
  -- traceShowM (map shelfName (take 1 shelves), boxesV, shelvesV, boxesV / shelvesV * 100)
  if shelvesV < 1e-2 
  then return 0
  else return $ boxesV / shelvesV

  

eigthV :: Int -> Char
eigthV n | n <= 0  = ' '
eigthV 1 = '▁'
eigthV 2 = '▂'
eigthV 3 = '▃'
eigthV 4 = '▄'
eigthV 5 = '▅'
eigthV 6 = '▆'
eigthV 7 = '▇'
eigthV _ = '█'

eigthH :: Int -> Char
eigthH n | n <= 0  = ' '
eigthH 1 = '▏'
eigthH 2 = '▎'
eigthH 3 = '▍'
eigthH 4 = '▌'
eigthH 5 = '▋'
eigthH 6 = '▊'
eigthH 7 = '▉'
eigthH _ = '█'


data Level = Empty -- blue
           | Low  -- green
           | Medium -- yellow
           | Used -- orange
           | Full -- red
     deriving (Show, Eq, Read,Enum,Bounded)

percToLevel :: Double -> Level
percToLevel x | x <= 1e-2 = Empty
percToLevel x | x <= 0.30 = Low
percToLevel x | x <= 0.60 = Medium
percToLevel x | x <= 0.90 = Used
percToLevel _             = Full

levelsToAttrName l1 l2 = attrName (show l1) <> attrName (show l2)
percToAttrName p1 p2 = levelsToAttrName (percToLevel p1) (percToLevel p2)

generateLevelAttrs :: [(AttrName, V.Attr)]
generateLevelAttrs = 
    [ (levelsToAttrName l1 l2, fg `on` bg)
    | l1 <- [minBound..maxBound]
    , l2 <- [minBound..maxBound]
    , let fg = levelToColor 255 l1
    , let bg= case l2 of 
                 Empty -> V.black 
                 _ -> levelToColor 127 l2
    ]
    
    
levelToColor v = \case
  Empty -> V.color240 0 v v -- cyan
  Low -> V.color240 0 v 0 -- green
  Medium -> V.color240 v v 0  -- yellow
  Used -> V.color240 v (v `div` 2) 0 -- orange
  Full -> V.color240 v 0 0 -- red




succ', pred' :: (Eq a, Enum a, Bounded a) => a -> a
succ' s | s == maxBound = minBound
succ' s = succ s
  
pred' s | s == minBound = maxBound
pred' s = pred s
  
makeStyleAttrName :: Text -> AttrName
makeStyleAttrName style = attrName "style" <> attrName (unpack style)

styleNameWithAttr style = withStyleAttr style (txt style)
withStyleAttr style w = withAttr (makeStyleAttrName style) w

defaultStyleAttrs :: [V.Attr]
defaultStyleAttrs = [ with $ fg `on` V.black
                    | with <- id : [] -- map withStyle [{- V.underline, -} V.italic]
                                  -- ++ [withStyle V.italic . withStyle V.underline ]
                    , fg <- [ V.red, V.green, V.yellow, V.blue, V.magenta, V.cyan
                            -- , V.brightRed, V.green, V.brightYellow, V.brightBlue, V.brightMagenta, V.brightCyan
                            ] ++
                            concat[ [ V.color240 a b c -- orange
                                    , V.color240 a c b
                                    , V.color240 b c a
                                    , V.color240 b a c
                                    , V.color240 c b a
                                    , V.color240 c a b
                                    ]
                                  | (a, b, c) <- [(0,100,200), (50,150,200), (0, 50,255)]
                                  ]
                    ]
  where withStyle = flip V.withStyle

                                 
