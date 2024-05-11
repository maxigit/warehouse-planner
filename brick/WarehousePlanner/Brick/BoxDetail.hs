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
import WarehousePlanner.Exec (execWH)
import System.IO.Unsafe (unsafePerformIO)



boxDetail :: Warehouse RealWorld -> HistoryRange -> ZHistory1 Box RealWorld -> Widget n
boxDetail _ _ ZHistory{..} | null zBefore && null zAfter = emptyWidget
boxDetail warehouse HistoryRange{..} ZHistory{..} = let
  history = if  hrCurrent >= hrToDiff
           then reverse $ Map.toList zBefore
           else reverse $ take 1 (Map.toList zBefore) ++ Map.toList zAfter
  (events, boxes) = unzip $ if hrCurrent == hrToDiff then take 2 history else history
  mk name f = withAttr bold_ (txt name) : mkDiffs f
  mkDiffs :: forall n . (Box RealWorld -> Text) -> [Widget n]
  mkDiffs f = [ case toDiffM of 
                 Nothing -> -- last diff
                            txt $ f current
                 Just toDiff -> renderDiffText (Just $ f current) (fmap f toDiff)
               | (current, toDiffM) <- zip boxes
                                          (drop 1 (map (Just . Just) boxes) ++ [Nothing])
               ]
  eventHeader = emptyWidget : map (withAttr bold_ . str . show) events
  pairs = [ take (length eventHeader) $ [ withAttr bold_ (txt "Style"),  styleNameWithAttr False $ boxStyle box] ++  repeat emptyWidget
          , mk "Location" (maybe "∅" (getShelfname warehouse) . boxShelf)
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


getShelfname :: Warehouse RealWorld -> ShelfId RealWorld -> Text
getShelfname warehouse shelfId = unsafePerformIO $ execWH warehouse do
    shelf <- findShelf shelfId
    return $ shelfName shelf
