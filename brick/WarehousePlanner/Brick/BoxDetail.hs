module WarehousePlanner.Brick.BoxDetail
( boxDetail
, boxDetailsTextTable
)
where 

import ClassyPrelude
import WarehousePlanner.Type
import WarehousePlanner.Base
import Brick qualified as B
import Brick (Widget, (<=>), hBox)
import Brick.Widgets.Table
import Brick.Widgets.Center
import Data.Char (isAlphaNum)
import Data.List.Split (chunksOf)
import WarehousePlanner.Brick.Util
import Data.Map qualified as Map
import WarehousePlanner.Exec (execWH)
import System.IO.Unsafe (unsafePerformIO)



boxDetail :: (Int -> Int -> Int) -> Warehouse RealWorld -> HistoryRange -> ZHistory1 Box RealWorld -> Widget n
boxDetail chunk warehouse hrange boxHistory = 
  case boxDetailTable warehouse hrange boxHistory of
       ([],_,_) -> B.emptyWidget
       (header, info, body)  -> let
           -- split the table into multiple tables which will be rendered side to side
           tables = chunksOf (chunk (length info) (length header)) (info <> body)
           box = headEx $ zBefore boxHistory ++ zAfter boxHistory
           in hCenter (B.txt (boxStyleAndContent box ) )
              <=> hBox ( map ( renderTable . rowBorders False . table)
                             ( map (header:) tables )
                       )
boxDetailTable :: Rendered w => Warehouse RealWorld -> HistoryRange -> ZHistory1 Box RealWorld -> ([w], [[w]], [[w]])
boxDetailTable _ _ ZHistory{..} | null zBefore && null zAfter = ([], [], [])
boxDetailTable warehouse HistoryRange{..} ZHistory{..} = let
  history = if  hrCurrent >= hrToDiff
           then reverse $ Map.toList zBefore
           else reverse $ take 1 (Map.toList zBefore) ++ Map.toList zAfter
  box = headEx $ zBefore ++ zAfter
  (events, boxes) = unzip $ if hrCurrent == hrToDiff then take 2 history else history
  -- mk :: Text -> ((Box RealWorld) -> Text) -> [_]
  mk name f = withAttrR bold_ (txtR name) : mkDiffs (Just . f)
  -- mkDiffs :: (Box RealWorld -> Maybe Text) -> [w]
  mkDiffs f' = let f =  fmap (take 20) . f'
               in [ case toDiffM of 
                 Nothing -> -- last diff
                            txtmR $ f current
                 Just toDiff -> renderDiffText (f current) (f toDiff)
               | (current, toDiffM) <- zip boxes
                                          (drop 1 (map Just boxes) ++ [Nothing])
               ]
  eventHeader = emptyR : map (withAttrR bold_ . txtR . tshow) events
  infoRows = [ take (length eventHeader) $ [ withAttrR bold_ (txtR "Style"),  styleNameWithAttr $ boxStyle box] ++  repeat emptyR
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
  -- tagRows :: [[_]]
  tagRows = map mkTagRow allTags -- $ filter nonVirtual allTags
  -- non standard tag at the end
  mkTagRow tag = let att = case (nonVirtual tag) of
                               True | special tag -> specialTagName_
                               True -> virtualTagName_
                               _ -> tagname_
                in withAttrR att (txtR tag) : mkDiffs (\box -> case getTagValuem box tag of
                                                                 Just "" -> Just "✓"
                                                                 v -> v
                                                    )
  in (eventHeader, infoRows , tagRows)

nonVirtual, special  :: Text -> Bool
nonVirtual tag = case uncons tag of
                  Just (c, _) -> not $ isAlphaNum c
                  _ -> False
special tag = case uncons tag of
                  Just (c, _) -> c == '@'
                  _ -> False

getShelfname :: Warehouse RealWorld -> ShelfId RealWorld -> Text
getShelfname warehouse shelfId = unsafePerformIO $ execWH warehouse do
    shelf <- findShelf shelfId
    return $ shelfName shelf
    
boxDetailsTextTable :: Warehouse RealWorld -> HistoryRange -> ZHistory1 Box RealWorld -> Text
boxDetailsTextTable warehouse hrange boxHistory  =
  case boxDetailTable warehouse hrange boxHistory of
       ([],_,_) -> emptyR
       (header, info, body)  -> let
           _box = headEx $ zBefore boxHistory ++ zAfter boxHistory
           in unlines -- $ boxStyleAndContent box
                      $ map renderRow (header : (info <> body))
  where renderRow = intercalate "\t"

