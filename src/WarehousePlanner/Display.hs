{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module WarehousePlanner.Display where

import ClassyPrelude
import WarehousePlanner.Base hiding(up, rotate)
import Diagrams.Prelude hiding(Box,offset,direction)
import Control.Monad.State(gets)
import Diagrams.Backend.Cairo 
-- import Data.Maybe
import Data.Map qualified as Map

display :: (Shelf s -> ShelfStyling) -> (Box s -> BoxStyling) -> WH (Diagram B) s
display shelfStyling boxStyling= do
    runs <- gets shelfGroup
    g <- renderRuns shelfStyling boxStyling runs
    s0 <- defaultShelf
    base <- renderShelf shelfStyling boxStyling s0
    return $ g === base
    
renderRuns shelfStyling boxStyling = renderGroup shelfStyling boxStyling vcat renderRun
renderRun shelfStyling boxStyling = renderGroup shelfStyling boxStyling hcat renderBay
renderBay shelfStyling boxStyling = renderGroup shelfStyling boxStyling (vcat . reverse) renderShelf

-- renderGroup :: (Shelf s -> ShelfStyling ) -> (Box s -> BoxStyling) -> RunsWithId s -> WH (Diagram B) s
renderGroup shelfStyling boxStyling cat_ renderChildren gs = do
    rendered <- mapM (renderChildren shelfStyling boxStyling) gs
    return $ pad (1.05) $ cat_ (map alignB $ toList rendered)
    
renderShelf :: (Shelf s -> ShelfStyling) -> (Box s -> BoxStyling) -> ShelfId s -> WH (Diagram B) s
renderShelf shelfStyling boxStyling shelfId = do            
    shelf <- findShelf shelfId
    (_, used) <- usedDepth shelf
    let styling@ShelfStyling{..}  = shelfStyling shelf
    let (Dimension (max 1 -> l) __w (max 1 -> h)) = maxDim shelf
        (Dimension ln wn hn) = minDim shelf
        lborder = lc border # lwL 2
        minborder = if (l,h) == (ln, hn)
                                 then id
                                 else lborder . dashingL [5, 2] 0
        rmax = rect l h # fc maxBackground
        rmin = rect ln hn # minborder # fc background
                          # lwL 0
                          # translate (r2 (- (l-ln)/2, -(h-hn)/2))
        -- r = rmin `atop` rmax
        r = t' `atop` rect l h # lborder `atop` rmin`atop` rmax  
        shelfTitle = case title of
          [] -> shelfName shelf
          _ -> intercalate "\n" title
        barTitle_ = fromMaybe (shelfName shelf) barTitle
        t = scaledText 50 20 barTitle_  #fc barForeground `atop` rect l 15 # fc (barBackground) # lc border # lwL 2
        -- display the depth bar relative to the full length,as that's what we are losing
        wn' = ln
        bar = if displayBarGauge then depthBar styling wn' (used*wn'/wn) else mempty
        bar' = (alignL bar # translateX 5) `atop` alignL t
        t' = scaledText l h shelfTitle #fc foreground 
        diagram = r -- t `atop` r
        align_ = case flow shelf of
                    LeftToRight -> alignBL
                    RightToLeft -> alignBR

    boxes <- renderBoxes boxStyling shelf
    

    return $ alignBL $ centerX ((align_ boxes) `atop` (align_ diagram)) === (centerX bar')

renderBoxes :: (Box s -> BoxStyling) -> Shelf s -> WH (Diagram B) s
renderBoxes boxStyling shelf = let 
    in do
        boxesNotSorted <- findBoxByShelf shelf
        let boxes = sortOn (downDepth . boxOffset) boxesNotSorted
            downDepth (Dimension l w h ) = (l, Down w, h)
        z'diags <-  mapM (renderBox boxStyling shelf) boxes
        let zMap = Map.fromListWith atop (concat z'diags)
        return $ foldl' atop (rect 0 0 ) (Map.elems zMap)
        --                   ^ necessary so to force the envelope to include (0,0)
        --                   so that the offset of boxes is not cancelled
        --                   when align diagrams

depthBar :: ShelfStyling -> Double -> Double -> Diagram B
depthBar ShelfStyling{..} w used = let
  shelfBar = depthBar'' (lc black . lwL 1) w 0 background
  usedBar = depthBar'' (lc black . lwL 1)  used 0 barBackground
  extraUsedBar = if used > w
                 then depthBar'' (lc black . lwL 1)  w 0 red
                 else mempty
  -- threshold1 = depthBar' (w*0.70) 0 red 
  -- threshold2 = depthBar' (w*0.85) 0 orange 
  in mconcat ( map alignL [ extraUsedBar 
                          , usedBar
                          -- , threshold1 
                          -- , threshold2 
                          , shelfBar 
                          ]
             )

gaugeBar :: Double -> Diagram B
gaugeBar w = let
  shelfBar = depthBar' w 0 green
  threshold1 = depthBar' (w*0.70) 0 red 
  threshold2 = depthBar' (w*0.85) 0 orange 
  in mconcat ( map alignL [ threshold1 
                          , threshold2 
                          , shelfBar 
                          ]
             )
depthBar' :: Double -> Double -> Colour Double -> Diagram B
depthBar' width_ offset colour = depthBar'' (lwL 1 . lc colour) width_ offset colour
-- depthBar' = depthBar'' (lwL 1) --  . lc colour) width_ offset colour
depthBar'' :: (Diagram B -> Diagram B) -> Double -> Double -> Colour Double -> Diagram B
depthBar'' up l l0 colour = translate (r2 (scale_ l0, 0)) . alignBL $ rect (scale_ l) 4 # fc colour # up
  where scale_ x = x /3

-- | render a Box within a shelf. 
-- Boxes are stack vertically until they reach the maximum.
-- to do so, we need to keep track of the postion of the last 
-- box

offsetBox :: Bool -> Shelf s -> Box s -> Diagram B -> Diagram B
offsetBox fromBoxCenter shelf box diagram  = let
    Dimension xn _yn _zn = minDim shelf
    -- Dimension xx yx zx = maxDim shelf
    Dimension l _w h = if fromBoxCenter then boxDim box else Dimension 0 0 0
    Dimension ox _oy oz = boxOffset box

    offset LeftToRight =  (ox+l/2, oz+h/2)
    offset RightToLeft = (xn -ox+l/2, oz+h/2) 

          
    in diagram # translate (r2 (offset (flow shelf)))

renderBox :: (Box s -> BoxStyling) -> Shelf s -> Box s -> WH [(Int, Diagram B)] s
renderBox boxStyling shelf box = do
    let BoxStyling{..} = boxStyling box
    let   Dimension l __w h = boxDim box
          border' = fromMaybe foreground border
          titles = case title of
                     [] -> (boxStyle box <> "\n" <> showOrientation (orientation box) <> " " <> boxContent box)
                     _ -> unlines title  
                        
    let   r = (r' $ rect l h  # lc border' # fc background # lwL 2) #scale 0.95 # pad 1.05
          r' r0 = pies (min l h / 3) circleBgs `atop` r0
          t = scaledText l h titles
              # fc foreground
          diagram_ = t `atop` r
          diagram = offsetBox True shelf box diagram_

          boxBar =  renderBoxBar box barTitle foreground background border' circleBgs
          backBag = if displayBarGauge
                    then renderBoxBarBg shelf
                    else mempty

          -- offset and scale to 
          Dimension _ ow' _ = boxCoordinate box
          oy = fromMaybe 0 offsetBarGaugeY * (ow' -1)
          ox = fromMaybe 0 offsetBarGaugeX * (ow' -1)
               

          offsetBar bar = offsetBox False shelf box $ bar # translate (r2 (5-ox,5-oy) ) 
    return $ [(3, diagram),
              (2, offsetBar backBag),
              (1, offsetBar boxBar)]
pies :: Double -> [Colour Double] -> Diagram B 
pies _ [] = mempty
pies radius (col1:colours) = let
  angle = 1  / fromIntegral (length colours + 1)
  dir0 = rotate (5/8 @@ turn) xDir
  ps = [ wedge radius dir (angle @@ turn) # fc col #lwL 0 
            | (col, i) <- zip colours [0..]
            , let dir = rotate (angle * i @@ turn) dir0
            ]
  base = circle radius # fc col1 # lwL 0
  in mconcat (ps ++ [base])

           

renderBoxBar :: Box s -> Maybe Text
             -> Colour Double -> Colour Double -> Colour Double -> [Colour Double]
             -> Diagram B
renderBoxBar box titlem foreground background border circleBgs =
  let Dimension _l w _h = boxDim box
      Dimension _ox oy _oz = boxOffset box
      c = pies (3/2) circleBgs
      t = maybe mempty (fc foreground . scaledText w w)  titlem  `atop` c
  in depthBar'' ((atop t) . (lwL 1 . lc $ blend 0.5  border background )) w oy (background)
  -- in depthBar'  w oy (background)

    
renderBoxBarBg :: Shelf s -> Diagram B
renderBoxBarBg shelf = gaugeBar yn
    where   Dimension _xn yn _zn = minDim shelf
    
-- | draw text scaled to fit given rectangle
scaledText :: Double -> Double-> Text -> Diagram B
scaledText x y s =  let
    (x0, y0) = (10,3) 
    (sX, sY) =  ((max 1 x)/x0, (max 1 y)/y0)
    in text (unpack s) & scale (min sX sY)


