{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module WarehousePlanner.Styling
( readFromPalette
, stylingFromTags
, shelfStylingFromTags
, valueToKolor, valueToKolorE
, Kolor
)
where

import Data.Colour.Palette.BrewerSet
import Data.Colour.Palette.ColorSet
import ClassyPrelude
import Prelude((!!))
import WarehousePlanner.Base
import Data.Colour (affineCombo)
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




-- | Get the RGB colour for a tagged object.
-- underscores are stripped before looking for the color name
-- this allow the same colours to be used more that once
-- example, navy#_navy#white,  will use navy twice
{- rST::colour
All property colours accept more than one colours (separated by ``;``)
which are colour names their hexadecimal description (for example
"white" or "ffffff" ) . If multiples colours are given they will be
mixed. However, each colour can only appear once (in the list of
colour). It is however possible to give more weight to a colour by
prefixing its name with a underscore. For example

::

   black#white => mid gray
   black#_black#white => dark gray
   black#_white#white => light gray

Colour palettes defined in haskell `palette
package <http://hackage.haskell.org/package/palette>`__ can be used. A
colour is represented by the palette name, the number of colour it
contains (if the palette of different variant) a dash and the colour
index starting a 1. Example

::

   YlGn4-1 -- first colour of the 4 colours variant of the YlGn palette.
   YlGn9-8 -- 8th colour of the 9 colours variant of the YlGn palette.
   wheel24-2 -- 2nd of a colour wheel

The palettes available are the one from the `Brewer
Set <http://hackage.haskell.org/package/palette-0.3.0.2/docs/Data-Colour-Palette-BrewerSet.html>`__
and the
`ColorSet <http://hackage.haskell.org/package/palette-0.3.0.2/docs/Data-Colour-Palette-ColorSet.html>`__.
Note that ``rybColor`` is name ``wheel24`` and the ``d3Colors`` 2 and
for used double indexing Example

::

   d3Colors44-1  first  colour of the bright version of d3Color4
   --       ^ bright
   d3Colors41-1  first  colour of the dark version of d3Color4
   --       ^ dark
::rST -}
colorFromTag :: HasTags tagged => Map Text Kolor -> tagged -> Text -> Maybe Kolor
colorFromTag colorMap box tag = let
  colors = mapMaybe (valueToColour colorMap) (getTagValues box tag)
  in case colors of
  [] -> Nothing
  [col] -> Just col
  (col:cols) -> let w = 1/fromIntegral (length colors) -- ALL colors
                in Just $ affineCombo (map (w,) cols) col

-- | Extract styling information from tag as properties
-- use fg= foregroung
{- rST::tags
Some special property are used to control how boxes and
shelves are displayed. For example assigning ``bg=red`` will set the background of
the box to red. Those properties are

-  ``bg`` background colour (shelves and boxes)
-  ``fg`` foreground (text) colour (shelves and boxes)
-  ``BG`` background colour of the maximum dimension (shelves)
-  ``border`` border colour (shelves and boxes)
-  ``circle``..\ ``circle4`` display a big circle of the given colour(s)
   on the box. Ideal to mark boxes. (boxes only). Displays pies if more
   than one colour is given.
-  ``title`` text to display instead of the box or shelf name (shelves
   and boxes)
-  ``bar-title`` text to display in the box bar (for each depth) or
   shelf bar.
-  ``bar-bg`` colour of the shelf bar (shelf bar).
-  ``bar-fg`` colour of the text of the shelf bar (shelf bar).
-  ``no-bar-gauge`` if present, disable the colour gauge behind the box
   bar or in the shelf bar.
-  ``bar-gauge-x`` if present, offset the bar gauge by x times the box
   index.
-  ``bar-gauge-y`` if present, offset the bar gauge by y times the box
   index.
::rST -}
stylingFromTags ::  Map Text Kolor -> Box s -> BoxStyling
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

shelfStylingFromTags :: Map Text Kolor -> Shelf s -> ShelfStyling
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
valueToColour :: Map Text Kolor -> Text -> Maybe Kolor
valueToColour colorMap t = case  dropWhile (== '_') t of
  "" -> Nothing
  col -> lookup col colorMap <|>  valueToKolor col

valueToKolor :: Text -> Maybe Kolor
valueToKolor t = case t of
  "" -> Nothing
  cs | all isHexDigit cs && length cs == 3 -> Just $ sRGB24read (unpack cs >>= (replicate 2))
  cs | all isHexDigit cs && length cs == 6 -> Just $ sRGB24read (unpack cs)
  dropped -> readFromPalette dropped
             <|> readColourName (map Data.Char.toLower $ unpack dropped)

valueToKolorE :: Text -> Either Text Kolor
valueToKolorE t = maybe (Left $ t <> " is not a valid colour name") Right (valueToKolor t)
-- | blend all colours equaly.
-- folding using normal blend would not work as
-- the weight of the last colour would count for half of everything
