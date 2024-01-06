{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module WarehousePlanner.Styling
( readFromPalette
, stylingFromTags
, shelfStylingFromTags
)
where

import Data.Colour.Palette.BrewerSet
import Data.Colour.Palette.ColorSet
import ClassyPrelude
import Prelude((!!))
import WarehousePlanner.Base
import Data.Colour (Colour,affineCombo)
import Data.Colour.Names (readColourName,black,lightgray,white, wheat, darkorange, lightsteelblue, royalblue, steelblue)
import Data.Colour.SRGB(sRGB24read)
import Data.Char(isHexDigit,toLower)


readFromPalette :: Text -> Maybe Kolor
readFromPalette name =
  case break (== '-')  name of
    (paletteName, (uncons -> Just ('-', indexs))) | Just i <- readMay indexs -> let
                              palettem = readBrewerSet paletteName <|> readColorSet paletteName
                              in palettem <*> (Just $ i-1)
    _ -> Nothing
-- * Brewer Set 
deriving instance Show ColorCat
deriving instance Enum ColorCat
deriving instance Bounded ColorCat

-- | Read brewer colour  using Palette#size-#colour
-- example "YlGn9-1"
-- We don't check that the palette size is within the boundary
readBrewerSet :: Text -> Maybe (Int -> Kolor)
readBrewerSet name =
  case [ (n, cat)
       | cat <- [minBound..maxBound] :: [ColorCat]
       , (stripPrefix  (tshow cat) -> (Just (readMay -> Just n))) <- [name]
       ] of
    [(n,  cat)] -> Just (\i -> brewerSet cat n !! (i `min` n))
    _ -> Nothing
    
-- * Read from Palette.ColorSet 
readColorSet :: Text -> Maybe (Int -> Kolor)
readColorSet "rybColor" = Just (\i -> rybColor (i `mod` 24))
readColorSet "wheel24" = Just (\i -> rybColor (i `mod` 24))
readColorSet "d3Colors1" = Just (\i -> d3Colors1 (i `mod` 10))
readColorSet "d3Colors21" = Just (\i -> d3Colors2 Light (i `mod` 10))
readColorSet "d3Colors22" = Just (\i -> d3Colors2 Dark (i `mod` 10))
readColorSet "d3Colors41" = Just (\i -> d3Colors4 Lightest (i `mod` 10))
readColorSet "d3Colors42" = Just (\i -> d3Colors4 Light (i `mod` 10))
readColorSet "d3Colors43" = Just (\i -> d3Colors4 Dark (i `mod` 10))
readColorSet "d3Colors44" = Just (\i -> d3Colors4 Darkest (i `mod` 10))
readColorSet _ = Nothing




-- underscores are stripped before looking for the color name
-- this allow the same colours to be used more that once
-- example, navy#_navy#white,  will use navy twice
colorFromTag :: HasTags tagged => Map Text Text -> tagged -> Text -> Maybe (Colour Double)
colorFromTag colorMap box tag = let
  colors = mapMaybe (valueToColour colorMap) (getTagValues box tag)
  in case colors of
  [] -> Nothing
  [col] -> Just col
  (col:cols) -> let w = 1/fromIntegral (length colors) -- ALL colors
                in Just $ affineCombo (map (w,) cols) col

-- | Extract styling information from tag as properties
-- use fg= foregroung
stylingFromTags ::  Map Text Text -> Box s -> BoxStyling
stylingFromTags colorMap box = let
  foreground = black `fromMaybe` (colorFromTag colorMap) box "fg"
  background = wheat `fromMaybe` (colorFromTag colorMap) box "bg"
  circleBgs = mapMaybe (colorFromTag colorMap box) ["circle", "circle2", "circle3", "circle4"]
  border = colorFromTag colorMap box "border"
  title = getTagValues box "title"
  barTitle= case getTagValues box "bar-title" of
               [] -> Nothing
               vs -> Just $ intercalate "\n" vs
  displayBarGauge = not (tagIsPresent box "no-bar-gauge")
  offsetBarGaugeX = getTagValuem box "bar-gauge-x" >>= readMay
  offsetBarGaugeY = getTagValuem box "bar-gauge-y" >>= readMay
  in BoxStyling{..}

shelfStylingFromTags :: Map Text Text -> Shelf s -> ShelfStyling
shelfStylingFromTags colorMap shelf = let
  isSeparator = tagIsPresent shelf "sep"
  foreground = black `fromMaybe` colorFromTag colorMap shelf "fg"
  background = (if isSeparator then white else lightsteelblue) `fromMaybe` colorFromTag colorMap shelf "bg"
  maxBackground = steelblue `fromMaybe` colorFromTag colorMap shelf "BG"
  border = royalblue `fromMaybe` colorFromTag colorMap shelf "border"
  barBackground = (if isSeparator then lightgray else darkorange) `fromMaybe` colorFromTag colorMap shelf "bar-bg"
  barForeground = black `fromMaybe` colorFromTag colorMap shelf "bar-fg"
  title = getTagValues shelf "title"
  barTitle= getTagValuem shelf "bar-title"
  displayBarGauge = not (tagIsPresent shelf "no-bar-gauge") && not isSeparator
  in ShelfStyling{..}
  
-- | Transform tag value to colours
-- Try to read standard name or use hexadecimal
valueToColour :: Map Text Text -> Text -> Maybe (Colour Double)
valueToColour colorMap t = case  dropWhile (== '_') t of
  "" -> Nothing
  col -> valueToColour' (findWithDefault col col colorMap)
valueToColour' t = case t of
  "" -> Nothing
  cs | all isHexDigit cs && length cs == 3 -> Just $ sRGB24read (unpack cs >>= (replicate 2))
  cs | all isHexDigit cs && length cs == 6 -> Just $ sRGB24read (unpack cs)
  dropped -> readFromPalette dropped
             <|> readColourName (map Data.Char.toLower $ unpack dropped)

-- | blend all colours equaly.
-- folding using normal blend would not work as
-- the weight of the last colour would count for half of everything
