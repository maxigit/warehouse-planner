module WarehousePlanner.Tiling 
( clampTilingMode
, howMany, howManyWithDiagonal
, howManyWithSplitH, howManyWithSplitV
, indexToOffsetDiag, d0, r
, tmBoundingBox
)
where
import ClassyPrelude hiding (unzip)
import WarehousePlanner.Base
import Data.List.NonEmpty (unzip)


clampTilingMode :: Maybe Int -> Maybe (Int, Int) -> Maybe Int -> TilingMode -> TilingMode
clampTilingMode maxLM' mWM' maxHM' mode' = fst $ go maxLM' mWM' maxHM' mode' where
  go maxLM mWM maxHM mode = 
    case mode of 
       Regular hmany -> mk Regular hmany
       Diagonal hmany d -> mk (flip Diagonal d) hmany
       TilingCombo Horizontal m1 m2 -> let
                    -- in horizontal mode, only "uses" length param
                    (m1', (maxLM', _, _)) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM' mWM maxHM m2
                    in (TilingCombo Horizontal m1' m2', next)
       TilingCombo Vertical m1 m2 -> let
                    -- in vertical mode, only "uses" length param
                    (m1', (_, _, maxHM')) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM mWM maxHM' m2
                    in (TilingCombo Vertical m1' m2', next)
       TilingCombo Depth m1 m2 -> let
                    -- in vertical mode, only "uses" length param
                    (m1', (_, mWM',_)) = go maxLM mWM maxHM m1
                    (m2', next) = go maxLM mWM' maxHM m2
                    in (TilingCombo Depth m1' m2', next)
    where clamp (HowMany _ nl nw nh) = let
                (minW, maxW) = unzip mWM
                hwmany = mkHowMany (minMaybe maxLM nl)
                                   (maxMaybe minW (minMaybe maxW nw))
                                   (minMaybe maxHM nh)
                nextParam = (fmap (\maxL -> maxL - perLength hwmany) maxLM
                            , fmap (\(_,maxW) -> (0, maxW - perDepth hwmany )) mWM
                            , fmap (\maxH -> maxH - perHeight hwmany) maxHM
                            )
                in (hwmany, nextParam)
          mk constr hw = let
             (hw', nextParams) = clamp hw
             in (constr hw', nextParams)
          minMaybe minm x = fromMaybe x  $ fmap (min x) minm
          maxMaybe maxm x = fromMaybe x  $ fmap (max x) maxm


-- | * test
{-
dx = Dimension 192 100 145
dn = Dimension 160 100 145
box = Dimension 47 39 85
os = [tiltedForward, tiltedFR]
-}


-- | How many inner rectangle can fit in the outer one ?
-- The outer min corresponds to the maximum of the n-1 box
-- This allows to set for example a maximum height for the bottom the last box
-- this is the maximum height a operator can reach the box
-- and the actual height of the shelf or ceiling, the physical limit.
--
--   max ----------------------
--           2    X   
--           2    X
--   min ____2____2___________
--           1    2
--           1    1
--           1    1
--
-- X is accepted even though it fit within max
-- but starts after min
--
howMany :: Dimension --  ^ Outer min
        -> Dimension -- ^ Out max
        -> Dimension --  ^ Inner
        -> HowMany
howMany (Dimension l0 w0 h0) (Dimension l w h) (Dimension lb wb hb) =
        mkHowMany ( fit l0 l lb)
                  ( fit w0 w wb)
                  ( fit h0 h hb)
        where
        fit d0 d1 db = 
          let d = min (d0+db) d1
          in floor (max 0 (d-0) /(db+0))


-- | Find how many boxes fits using a "diagnal trick", when one box is rotated the other along a diagoral.
-- For example
--    1477
--    14 8
--    2558
--    2 69
--    3369
--  3, 5 and 7 box are rotated down allowing f
howManyWithDiagonal :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithDiagonal minOuter outer inner@(Dimension lb _ hb) | lb == hb =
  Regular $ howMany minOuter outer inner
howManyWithDiagonal minOuter outer@(Dimension l _ h) inner@(Dimension lb _ hb) | lb < hb = 
  let normal@(HowMany _ _ wn hn) = howMany minOuter outer inner
      fit d db = floor (max 0 (d-0) /(db+0))
      -- how many extra can we squeeze horizontally
      -- if we turn the box
      diff = hb - lb -- >0 
      -- check how many squares actually fit height wise
      minSquareNumberV = max 1 do  (diff - 1 + fromIntegral (hn+1) * hb - h) `fit` diff
      maxSquareWidth = case minSquareNumberV of
                       0 -> hn 
                       _ -> (hn + 1) `div` minSquareNumberV
      nForDiag n =
         --  | = =  | = =
         --
         --  = = |  = = |     1 square x 2
         --  = | =  = | =
         --  | = =  | = =
         --
         let squareL = fromIntegral (n-1)*lb+hb
             squareH = fromIntegral (n-1)*hb+lb
             sqNL = fit l squareL
             sqNH = fit h squareH
             leftL = l - squareL * fromIntegral sqNL
             leftH = h - squareH * fromIntegral sqNH
             mb = hb -- max lb hb
             -- find how many row/column can we use
             -- in a partial square
             -- We use mb because the first row/column
             -- will use both orientation for one of the box
             leftOver remaining b =
              if remaining < mb
              then 0
              else -- traceShow ("Remaining", remaining, remaining-mb, "b" , b)
                   fit (remaining -  mb) b +1
              
         in Diagonal (mkHowMany (sqNL * n + leftOver leftL lb)
                                 wn
                                 (sqNH * n + leftOver leftH hb)
                      )
                      n
   -- in case (min maxSquareNumberH minSquareNumberV , [max 2 minSquareWidth .. maxSquareWidth]) of
   in case (minSquareNumberV, [2 .. maxSquareWidth]) of
         (0, _) -> Regular normal
         (_, []) -> Regular normal
         (_, ds) -> let options = map nForDiag ds
                        bests = (Regular normal): options
                    in if outer /= minOuter
                       then Regular normal
                       else case bests of
                        [] -> error "Shouldn't happen"
                        _ -> minimumEx bests
howManyWithDiagonal minOuter outer inner = 
  rotateTM $ howManyWithDiagonal (r minOuter) (r outer) (r inner) where
  r = rotate tiltedRight

-- | Find how many  boxes  fit using two regular tiling as 
-- For example
--  44 55 66 7 8
--  11 22 33 7 8
-- At the moment tries different diagonol layout.
howManyWithSplitH :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithSplitH minOuter outer inner =  let
  tmode = Regular $ howMany minOuter outer inner 
  currentTotal = tmTotal tmode
  tries = [ TilingCombo Horizontal leftMode rightMode
          | i <- [tmLength tmode `div` 2 .. tmLength tmode ]
          -- we only need to try half of the solution, because
          -- the other will be tested when we try the rotated box
          , let leftMode = clampTilingMode (Just i) Nothing Nothing tmode
          , let bbox = tmBoundingBox leftMode inner
          , let used = invert $ Dimension (dLength bbox) 0 0
          , let rightMode =  howManyWithDiagonal (minOuter <> used) (outer <> used) (rotate tiltedRight inner)
          , tmTotal rightMode + tmTotal leftMode > currentTotal
          ]
  in minimumEx (tmode: tries)
howManyWithSplitV :: Dimension -> Dimension -> Dimension -> TilingMode
howManyWithSplitV minOuter outer inner = let
  rot = rotate tiltedRight 
  in rotateTM $ howManyWithSplitH (rot minOuter) (rot outer) (rot inner)
  
rotateTM (Regular hmany) = Regular (rotateH hmany)
rotateTM (Diagonal hmany n) = Diagonal (rotateH hmany) n
rotateTM (TilingCombo dir m1 m2) = let
         dir' = case dir of
                 Horizontal -> Vertical
                 Vertical -> Horizontal
                 Depth -> Depth
         in TilingCombo dir' (rotateTM m1) (rotateTM m2)
rotateH (HowMany n l w h) = HowMany n h w l
  
-- | Given a box and a tiling mode, returns the bounding box
tmBoundingBox :: TilingMode -> Dimension -> Dimension
tmBoundingBox (Regular (HowMany _ l w h)) (Dimension lb wb hb) =
  Dimension (fromIntegral l * lb)
            (fromIntegral w * wb)
            (fromIntegral h * hb)
tmBoundingBox (Diagonal (HowMany _ l w h) d) box =
  fst $ indexToOffsetDiag box d (l+1, w+1, h+1)
tmBoundingBox (TilingCombo dir m1 m2) box = let
  Dimension l1 w1 h1 = tmBoundingBox m1 box
  Dimension l2 w2 h2 = tmBoundingBox m2 (rotate tiltedRight box)
  in case dir of
       Horizontal -> Dimension ((+) l1 l2) (max w1 w2) (max h1 h2)
       Depth -> Dimension (max l1 l2) ((+) w1 w2) (max h1 h2)
       Vertical -> Dimension (max l1 l2) (max w1 w2) ((+) h1 h2)



                 


















d0, r :: Dimension
d0 = Dimension 0 0 0
r = Dimension 13 1 10
-- | Computes position and orientation of a box within a "Diagonal" pattern
-- see `howManyWithDiagonal`
indexToOffsetDiag :: Dimension -> Int -> (Int, Int, Int) -> (Dimension, Bool)
indexToOffsetDiag (Dimension l w h) diagSize (il, iw, ih) =
  let (lq, lr) = il `divMod`  diagSize
      (hq, hr) = ih `divMod` diagSize
      --    
      --    b   X|    X
      --      X  |  X c
      --    X   a|X  
      -- lq  0    1     2
      --    
      -- X) turned
      -- a) (il, ih) = (2,0)
      --    (lq, lr) = (0,2) 1 before
      --    (hq, hr) = (0,0) 0 below
      --    
      --
      -- c) (il, ih) = (5,1)
      --    (lq, lr) = (1,2) 2 before
      --    (hq, hr) = (0,1) 0 below
      (turnedBefore, turnedBelow, turned) =
        if lr > hr
        then -- left of the diagonal turnedBelow +1
          (lq+1, hq, False)
        else if lr ==  hr -- on the diagnoal
        then
          (lq, hq, True)
        else -- right of diagonal
          (lq, hq+1, False)
  in  (Dimension (fromIntegral (il-turnedBefore) * l + fromIntegral turnedBefore*h)
                 (fromIntegral iw * w)
                 (fromIntegral (ih - turnedBelow) * h + fromIntegral turnedBelow*l)
      , turned
      )
          

_roundDim :: Dimension -> [Int]
_roundDim (Dimension l w h) = map (round . (*100)) [l,w,h]

