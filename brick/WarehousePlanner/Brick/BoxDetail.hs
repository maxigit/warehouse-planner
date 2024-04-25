module WarehousePlanner.Brick.BoxDetail
( boxDetail
)
where 

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Base
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center
import Data.Char (isAlphaNum)
import Data.List.Split (chunksOf)
import WarehousePlanner.Brick.Util



boxDetail :: Box RealWorld -> Widget n
boxDetail box@Box{..} = let
  mk name value = [withAttr bold_ (txt name), txt value]
  pairs = [ [ withAttr bold_ (txt "Style"),  styleNameWithAttr False boxStyle]
          , mk "Content" boxContent
          , mk "Position" (boxPositionSpec box)
          , mk "Dimension" (printDim _boxDim)
          , mk "Offset" (printDim boxOffset)
          , mk "Possible Ors" (intercalate "" $ map showOrientation' boxBoxOrientations)
          , mk "Priorites" (tshow boxPriorities)
          , mk "Break" (maybe "" tshow boxBreak)
          ]
  tags = [ [mkTag t, txt val]
         | (tag, v) <- sortOn (nonVirtual . fst) $ mapToList boxTags
         , let values = toList v
         , (t,val) <- zip (tag: repeat "") values
         ]
  -- non standard tag at the end
  nonVirtual tag = case uncons tag of
                     Just (c, _) -> not $ isAlphaNum c
                     _ -> False
  special tag = case uncons tag of
                     Just (c, _) -> c == '@'
                     _ -> False
  mkTag tag = let att = case (nonVirtual tag) of
                               True | special tag -> specialTagName_
                               True -> virtualTagName_
                               _ -> tagname_
                in withAttr att $ txt tag
  tagss = chunksOf (length pairs) tags
  in hCenter (txt (boxStyleAndContent box ) )
     <=> hBox ( map ( renderTable . rowBorders False . table)
                    ( pairs : tagss)
              )


