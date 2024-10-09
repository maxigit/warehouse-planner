{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}
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
, selectedAttr, selectAttr
, bayNameAN
, hBoxB, vBoxB
, bold_, boldAttr
, tagname_, tagNameAttr
, virtualTagName_, virtualTagAttr
, specialTagName_, specialTagAttr
, eventAttrs
, historyIndicator
, isInSummary
, eventTree
, displayEventRange 
, renderDiffText
, hlToAttr
, withHLStatus
, highlightAttrs
, withBoxHLStatus, withShelfHLStatus
, boxPropValue
, gradientAttributes
, Rendered(..)
, grayAttr
) where

import ClassyPrelude hiding (on)
import WarehousePlanner.Base
import WarehousePlanner.Summary
import WarehousePlanner.History (findPreviousSibling, findNextSibling, findNextEvent)
import Brick
import Brick.Widgets.Border
import Graphics.Vty.Attributes qualified as V
import Data.Set qualified as Set
import WarehousePlanner.History (diffFor)
import WarehousePlanner.Brick.Types
import Data.Bits ((.|.))
import Data.List (unfoldr, nub)
import Data.List qualified as List

-- * Output Text or Widget
-- | Output to a text (for file) or Widget
class Rendered w where
    txtR :: Text -> w
    emptyR :: w
    txtmR :: Maybe Text -> w
    withAttrR, withDefAttrR :: AttrName -> w -> w
    {-# MINIMAL withAttrR , withDefAttrR,  (txtmR | (txtR, emptyR)) #-}
    default txtR :: Text -> w
    txtR = txtmR . Just
    default emptyR :: w
    emptyR = txtmR Nothing
    default txtmR :: Maybe Text -> w
    txtmR = maybe emptyR txtR

instance Rendered Text where
   txtR = id
   emptyR = ""
   withAttrR _  t = t
   withDefAttrR _ t = t
   
instance Rendered (Widget n) where
   txtR = txt
   emptyR = emptyWidget
   withAttrR = withAttr
   withDefAttrR = withDefAttr
    

-- * Percentatge
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
eigthV n | n <= 0  = '·'
eigthV 1 = '▁'
eigthV 2 = '▂'
eigthV 3 = '▃'
eigthV 4 = '▄'
eigthV 5 = '▅'
eigthV 6 = '▆'
eigthV 7 = '▇'
eigthV 8 = '█'
eigthV _ = '▲'

eigthH :: Int -> Char
eigthH n | n <= 0  = '·'
eigthH 1 = '▏'
eigthH 2 = '▎'
eigthH 3 = '▍'
eigthH 4 = '▌'
eigthH 5 = '▋'
eigthH 6 = '▊'
eigthH 7 = '▉'
eigthH 8 = '█'
eigthH _ = '▶'


data Level = Empty -- blue
           | Low  -- green
           | Medium -- yellow
           | Used -- orange
           | Full -- dark red
           | TooMuch -- red
     deriving (Show, Eq, Read,Enum,Bounded)

percToLevel :: Double -> Level
percToLevel x | x <= 1e-2 = Empty
percToLevel x | x <= 0.30 = Low
percToLevel x | x <= 0.60 = Medium
percToLevel x | x <= 0.90 = Used
percToLevel x | x <= 1 = Full
percToLevel _             = TooMuch

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
  Empty -> V.color240 0 v4 v4 -- cyan
  Low -> V.color240 0 v 0 -- green
  Medium -> V.color240 v v 0  -- yellow
  Used -> V.color240 v v2 0 -- orange
  Full -> V.color240 v2 0 0 -- red
  TooMuch -> V.color240 v 0 v -- red
  where v2 = v `div` 2
        v4 = v `div` 4




succ', pred' :: (Eq a, Enum a, Bounded a) => a -> a
succ' s | s == maxBound = minBound
succ' s = succ s
  
pred' s | s == minBound = maxBound
pred' s = pred s
  
makeStyleAttrName :: Text -> AttrName
makeStyleAttrName style = attrName "style" <> attrName (unpack style)

styleNameWithAttr :: Rendered w => Text -> w
styleNameWithAttr style = withStyleAttr style (txtR style)
withStyleAttr style w = withDefAttrR (makeStyleAttrName style) w

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
                    
gradientAttributes :: [Text] -> [(AttrName, V.Attr)]
gradientAttributes [] = []
gradientAttributes [name] = [(makeStyleAttrName name, V.red `on` V.black)]
gradientAttributes names = let
  n = length names
  ng = n `div` 4
  ny = ng
  nr = ny
  np = n - ng - ny - nr
  range _ end 0 _ = end
  range start end nr i = start + ((end - start) * i `div` nr)
  greenToYellow = [ V.srgbColor (range 0 255 ng i) 255 0
                  | i <- [0..ng-1]
                  ]
  yellowToRed = [ V.srgbColor 255 (range 255 0 ny i) 0
                | i <- [0..ny-1]
                ]
  redToPurple = [ V.srgbColor 255 0 (range 0 255 nr i)
                | i <- [0..nr-1]
                ]
  purpleToCyan = [ V.srgbColor (range 255 0 (np-1) i) (range 0 255 (np-1) i) 255
                | i <- [0..np-1]
                ]
                --                          ^^^^
                -- so that we reach 255,
                -- not needed for the other range
                -- because the last color of a range
                -- is the first of the next one
  colors = greenToYellow
         <> yellowToRed
         <> redToPurple
         <> purpleToCyan
  in [ (makeStyleAttrName name,  fg `on` V.black)
     | (name, fg) <- zip names colors
     ]





selected_ = attrName "selected"
selectedAttr = (selected_, V.defAttr `V.withStyle` V.reverseVideo)
                                 
selectAttr b w = 
  if b
  then withAttr selected_ w
  else w
  
bayNameAN = (attrName "shelfname", V.white `on` V.black) --  V.black `on` V.color240 255 128 0)

bold_ = attrName "bold"
boldAttr = (bold_, V.defAttr `V.withStyle` V.bold)


tagname_ = attrName "tagname"
tagNameAttr = (tagname_, snd boldAttr)
virtualTagName_ = attrName "vtagname"
virtualTagAttr = (virtualTagName_, (V.red `on` V.black))
specialTagName_ = attrName "stagname"
specialTagAttr = (specialTagName_, (V.yellow `on` V.black))

hBoxB :: [Widget n] -> Widget n
hBoxB = hBox . intersperse vBorder 


vBoxB :: [Widget n] -> Widget n
vBoxB = vBox . intersperse hBorder 



-- * History
historyIndicator :: Ord a => Widget n -> (a -> Bool) -> HistoryRange -> Map Event (DiffStatus (Set a)) -> Widget n
historyIndicator def summary hrange eventMap =
  case diffFor hrange eventMap of
     -- (_,status) | status == mempty -> def
     (_,status) ->  renderDiffStatus def summary status
     
renderDiffStatus :: Ord a => Widget n -> (a -> Bool) -> DiffStatus (Set a) -> Widget n
renderDiffStatus def inSummary (DiffStatus{..}) = let
  isIn = not . null $ Set.filter inSummary dsBoxIn
  isOut = not . null $ Set.filter inSummary dsBoxOut
  in if
     | isOut && isIn   -> withAttr eventIOut $ str "@"
     | dsBoxDeleted > 0 -> withAttr eventIn $ str "-"
     | isOut             -> withAttr eventOut $ str "<"
     | dsBoxCreated > 0 -> withAttr eventIn $ str "+"
     | isIn              -> withAttr eventIn $ str ">"
     | dsBoxShuffled > 0  -> withAttr eventUpdated $ str "@"
     | dsBoxUpdated > 0  -> withAttr eventUpdated $ str "#"
     | otherwise         -> def
  
isInSummary :: Text -> Text -> Bool
isInSummary summaryName name = not (null summaryName) 
                             && summaryName `isPrefixOf` name
    
eventUpdated = attrName "event" <> attrName "updated"
eventIn = attrName "event" <> attrName "in"
eventOut = attrName "event" <> attrName "out"
eventIOut = attrName "event" <> attrName "iout"

eventAttrs = [(ev, V.black `on` fg)
             | (ev, fg) <- [ (eventUpdated, V.cyan)
                           , (eventIn, V.green  )
                           , (eventOut, V.red)
                           ,(eventIOut, V.yellow)
                           ]
             ]
             -- <>
             -- [ (eventUpdated, V.blue `on` V.cyan)
             -- , (eventIn, V.brightGreen `on` V.green )
             -- , (eventOut, V.brightRed `on` V.red )
             -- ]

grayAttr = V.color240 150 150 150 `on` V.black

highlightAttrs :: [(AttrName, V.Attr)]
highlightAttrs = [ (attrName "hl" , V.defAttr `V.withStyle` V.reverseVideo)
                 , (attrName "current", V.defAttr `V.withStyle` (V.underline .|. V.bold))
                 , (attrName "hl" <> attrName "current", V.defAttr `V.withStyle` (V.reverseVideo .|. V.underline .|. V.bold))
                 -- , (attrName "selected", V.defAttr `V.withStyle` V.underline)
                 , (attrName "selected", V.defAttr `V.withStyle` V.reverseVideo)
                 , (attrName "hl" <> attrName "selected", V.defAttr `V.withStyle` (V.bold .|. V.reverseVideo))
                 , (attrName "current" <> attrName "selected", V.defAttr `V.withStyle` (V.reverseVideo .|. V.underline))
                 ]

eventTree :: Event -> Widget n
eventTree = go "" . Just where
   go tab (Just ev)  = (str tab <+> txt (displayEvent ev)) <=> go (tab ++ "  ") (evParent ev)
   go _ Nothing = emptyWidget

displayEventRange :: [Event] -> Bool -> Event -> Event -> Widget n
displayEventRange all current start end = let
  [beg',last'] = sort [start, end]
  _beg = fromMaybe beg' $ asum $  filter (/= (Just NoHistory)) [ findPreviousSibling beg' all , evParent beg' , evPreviousM beg' ]
  beg = List.last $  beg' : take 10 (unfoldr (fmap (\c -> (c, c)) . evPreviousM) beg')
  _last = fromMaybe last' $ findNextSibling last' all <|> findNextEvent last' all
  last = List.last $ last' : take 10 (unfoldr (fmap (\c -> (c, c)) . flip findNextEvent all) last')
  range = reverse $ unfoldr previousIf last
  previousIf ev = case evPreviousM ev of
                       Just p | p > beg -> Just (p,p)
                       _ -> Nothing
  events = nub $ sort $ beg : range ++ [_beg, _last, last]
  level ev = case ev of 
               NoHistory -> 0
               _ -> case evLevel ev of
                         l | l < 10 -> l
                         l | l < 100 -> 11
                         _ -> 12


-- W => hl
-- D => current
  render ev = let w = withAttr (specialTagName_) (txt (take (level ev) ".   .    . |         ")) <+> txt (displayEvent ev)
              in if | ev == start && ev == end  -> forceAttr eventIOut$ str "* " <+> w
                    | ev == start && current  -> forceAttr eventIn $ str "W " <+> w
                    | ev == start -> (forceAttr eventIn $ str "W ") <+> w
                    | ev == end && not current -> forceAttr eventOut $ str "D " <+> w
                    | ev == end            -> (forceAttr eventOut $ str "D ") <+> w
                    | otherwise -> str "  " <+> w
  in vBox $ map render events

  

renderDiffText :: Rendered w => Maybe Text ->  Maybe Text -> w
renderDiffText valuem oldm = 
  case (valuem, oldm) of
       (Nothing, Nothing) -> emptyR
       (Nothing, Just _) -> withAttrR eventOut $ txtR "∅" -- deleted
       (Just value, Nothing) -> withAttrR eventIn $ txtR value
       (Just value, Just old) | value /= old -> withAttrR eventUpdated $ txtR value
       (Just value, Just _) {- | value == old -} -> txtR value



hlToAttr :: HighlightStatus -> AttrName
hlToAttr HighlightStatus{..}  = mconcat
   [ if hsHighlighted > 0 then attrName "hl" else mempty
   , if hsCurrent then attrName "current" else mempty
   , if hsSelected > 0 then attrName "selected" else mempty
   ]
   
withHLStatus :: HighlightStatus -> Widget n -> Widget n
withHLStatus hs = withAttr (hlToAttr hs)

withShelfHLStatus :: ShelvesSummary SummaryExtra f a  -> Widget n -> Widget n
withShelfHLStatus ssum = withAttr (hlToAttr . seShelfHLStatus $ sExtra ssum)

withBoxHLStatus :: ShelvesSummary SummaryExtra f a  -> Widget n -> Widget n
withBoxHLStatus ssum = withAttr (hlToAttr . seBoxHLStatus $ sExtra ssum)


boxPropValue :: Box s -> Text
boxPropValue box = fromMaybe (boxStyle box) $ getTagValuem box "@prop"

