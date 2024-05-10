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
import Data.Map qualified as Map



boxDetail :: HistoryRange -> ZHistory1 Box RealWorld -> Widget n
boxDetail _ ZHistory{..} | null zBefore && null zAfter = emptyWidget
boxDetail HistoryRange{..} ZHistory{..} = let
  history = if  hrCurrent >= hrToDiff
           then reverse $ Map.toList zBefore
           else reverse $ take 1 (Map.toList zBefore) ++ Map.toList zAfter
  (events, boxes) = unzip history
  mk name f = withAttr bold_ (txt name) : mkDiffs f
  mkDiffs :: forall n . (Box RealWorld -> Text) -> [Widget n]
  mkDiffs f = [ renderDiffText (Just $ f current) (fmap f toDiff)
               | (current, toDiff) <- zip boxes
                                          (drop 1 (map Just boxes) ++ [Nothing])
               ]
  eventHeader = emptyWidget : map (withAttr bold_ . str . show) events
  pairs = [ take (length eventHeader) $ [ withAttr bold_ (txt "Style"),  styleNameWithAttr False $ boxStyle box] ++  repeat emptyWidget
          , mk "Location" (tshow . boxShelf)
          , mk "Content" boxContent
          , mk "Position" (boxPositionSpec)
          , mk "Dimension" (printDim . _boxDim)
          , mk "Offset" (printDim . boxOffset)
          , mk "Possible Ors" (intercalate "" . map showOrientation' . boxBoxOrientations)
          , mk "Priorites" (tshow . boxPriorities)
          , mk "Break" (maybe "" tshow . boxBreak)
          ]
  allTags :: [Text]
  allTags = keys $ foldMap boxTags boxes
  tags :: forall n . [[Widget n]]
  tags = map mkTag allTags -- $ filter nonVirtual allTags
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
                in withAttr att (txt tag) : mkDiffs (\box -> fromMaybe "∅" $ getTagValuem box tag)
  tagss = chunksOf (length pairs) tags
  box = headEx $ zBefore ++ zAfter
  in hCenter (txt (boxStyleAndContent box ) )
     <=> hBox ( map ( renderTable . rowBorders False . table)
                    ( map (eventHeader:) (pairs : tagss))
              )


