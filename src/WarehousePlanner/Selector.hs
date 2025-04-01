module WarehousePlanner.Selector
( applyNameSelector
, applyTagSelectors
, matchName
, parseBoxSelector
, parseBoxSelectorWithDef
, parseBoxNumberSelector
, parseShelfSelector
, parseSelector
, parseNameSelector
, parseMatchPattern
, parseTagSelector
, printSelector, printBoxSelector, printShelfSelector
, MParser
, between
, Selectable(..)
, splitOnNonEscaped
) where
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import WarehousePlanner.Type
import System.FilePath.Glob qualified as Glob
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Map.Lazy qualified as Map
import Data.Text (breakOn, uncons, stripPrefix)
import Control.Monad.Fail 
import Data.Void(Void)

type MParser = P.Parsec Void Text

-- * Selectors 
-- ** Applying 
-- | The phantom type guarantie that we are selecting the correct item
applyNameSelector :: NameSelector a -> (a s -> Text) -> a s -> Bool
applyNameSelector (NameMatches []) _ _ = True
applyNameSelector (NameMatches pats) name o = any (flip applyPattern (name o)) pats
applyNameSelector (NameDoesNotMatch pats) name o = not $ any (flip applyPattern (name o)) pats


applyTagSelector :: TagSelector s -> Tags -> Bool
applyTagSelector (TagHasKey pat) tags = case pat of
  MatchFull key -> key `member` tags
  MatchAnything -> True
  MatchGlob glob -> not (null ks) where ks = filter (Glob.match glob . unpack) (keys tags)
applyTagSelector (TagHasNotKey pat) tags = not $ applyTagSelector (TagHasKey pat) tags
applyTagSelector (TagIsKey pat) tags = case pat of
  MatchFull key -> lookup key tags == Just mempty
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [__one] -> True
    _ -> False
applyTagSelector (TagIsKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndAll (unRefTag tags valuePats)  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndAll (unRefTag tags valuePats) values
    _ -> False
  _ -> False
applyTagSelector (TagHasKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndSome (unRefTag tags valuePats)  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndSome (unRefTag tags valuePats) values
    _ -> False
  _ -> False
applyTagSelector (TagHasValues valuePat) tags = let
  tagValues = mconcat (Map.elems tags)
  in matchesAllAndSome (unRefTag tags valuePat) tagValues
applyTagSelector (TagHasNotValues valuePat) tags = not $ applyTagSelector (TagHasValues valuePat) tags
applyTagSelector (TagHasKeyAndNotValues key valuePat) tags = not (applyTagSelector (TagHasKeyAndValues key valuePat) tags)

applyTagSelectors :: Show (a s) => [TagSelector a] -> (a s -> Tags) -> a s -> Bool
applyTagSelectors [] _ _ = True
applyTagSelectors selectors tags o =  all (flip applyTagSelector (tags o)) selectors

unRefTag :: Tags -> [ValuePattern] -> [MatchPattern]
unRefTag tags = concatMap \case 
         VMatch m -> [m]
         VTag tag -> maybe [] (map MatchFull . setToList) $ lookup tag tags

-- | Check all pattern are matched and matches all values
matchesAllAndAll :: [MatchPattern] -> Set Text -> Bool
matchesAllAndAll pats vals = case unmatched pats vals of
  ([], []) -> True
  _ -> False 
-- | Check all pattern matches a value (but not all values have to be matches)
matchesAllAndSome :: [MatchPattern] -> Set Text -> Bool
matchesAllAndSome pats val = case unmatched pats val of
  ([], _) -> True
  _ -> False

unmatched :: [MatchPattern] -> Set Text -> ([MatchPattern], [Text])
unmatched pats0 valSet = go [] pats0 (Set.toList valSet) where
  go unused pats [] = (pats <> unused , [])
  go unused [] vals = (unused, vals)
  go unused (pat:pats) vals = case List.partition (applyPattern pat) vals of
    ([], _) -> go (pat:unused) pats vals
    -- \^ doesn't match anything, add to unused
    (_, vals') -> go unused pats vals'

-- ** Class
class Selectable a where
   applySelector :: Selector a -> a s -> Bool
   
instance Selectable Box where
   applySelector Selector{..} box = applyNameSelector nameSelector boxStyle box
                                  && applyTagSelectors tagSelectors boxTags box
   
   
instance Selectable Shelf where
   applySelector Selector{..} shelf = applyNameSelector nameSelector shelfName shelf
                                    && applyTagSelectors tagSelectors shelfTag shelf
                                    
  
-- ** Parsing 
-- | split on |
parseSelector :: Text -> Selector a
parseSelector s = case splitOnNonEscaped "#" s of
  [] -> Selector(NameMatches []) []
  (name:tags) -> Selector (parseNameSelector name) (mapMaybe parseTagSelector tags)

parseNameSelector :: Text -> NameSelector a
parseNameSelector selector = let
  (constr, pat) = case uncons selector of
       Just ('!', sel) -> (,) NameDoesNotMatch sel
       _ ->  (,) NameMatches selector
  in constr $ map parseMatchPattern (splitOnNonEscaped "|" pat)

parseTagSelector :: Text -> Maybe (TagSelector s)
parseTagSelector tag | null tag =  Nothing
parseTagSelector tag = Just $ case break ('='  ==) tag of
  (key, "")  -> case uncons key of
                Just ('-', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                Just ('!', nokey) -> TagHasNotKey $ parseMatchPattern nokey
                _ -> TagIsKey $ parseMatchPattern key
  ("", stripPrefix "=-" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  ("", stripPrefix "=" -> Just values) -> TagHasValues  (mkValues values)
  -- ("", stripPrefix "=!" -> Just values) -> TagHasNotValues  (mkValues values)
  (key, stripPrefix "=+" -> Just values) -> TagHasKeyAndValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=-" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=!" -> Just values) -> TagHasKeyAndNotValues (parseMatchPattern key) (mkValues values)
  (key, stripPrefix "=" -> Just values) -> TagIsKeyAndValues (parseMatchPattern key) (mkValues values)
  _ -> error "Bug. Result of break = should start with = or being captured earlier"
  where mkValues = map parseVPattern . fromList . splitOnNonEscaped ";"
  
parseMatchPattern :: Text -> MatchPattern
parseMatchPattern "" = MatchAnything
parseMatchPattern "*" = MatchAnything
parseMatchPattern pat | isGlob pat= MatchGlob (Glob.compile $ unpack pat)
parseMatchPattern pat = MatchFull pat
  
parseVPattern :: Text -> ValuePattern
parseVPattern (stripPrefix "$[" -> Just pat)   | Just tag <- stripSuffix "]" pat = VTag tag
parseVPattern pat = VMatch $ parseMatchPattern pat


parseBoxSelector :: Text -> BoxSelector
parseBoxSelector = parseBoxSelectorWithDef True
-- | "...^" is synomym for ...^@^@^@
parseBoxSelectorWithDef :: Bool -> Text -> BoxSelector
parseBoxSelectorWithDef defUseBase "*" = BoxSelector SelectAnything SelectAnything (parseBoxNumberSelector defUseBase "")
parseBoxSelectorWithDef defUseBase selector = let
  (box'location, numbers) = break (=='^') selector
  (box, drop 1 -> location) = break (=='/') box'location
  in BoxSelector (parseSelector box)
                 (parseSelector location)
                 $ case numbers of 
                    "^" -> parseBoxNumberSelector True "^^"
                    _ -> parseBoxNumberSelector defUseBase $ drop 1 numbers

parseShelfSelector :: Text -> ShelfSelector
parseShelfSelector selector = let
  BoxSelector boxSel shelfSel _ = parseBoxSelector selector
  in ShelfSelector boxSel shelfSel

{- rST::number-selector
The ``^`` symbol is used to select only a certain number of boxes per
variant/content, per shelf, and in total

::

   ^ content ^ shelf ^ total

Example

::

   TShirt^^^1 => the first box containing a t-shirt.
   ^1 => The first box of each variations (colour)
   ^2^3 =>  The two first boxes of each variations, with a maximum of 3 per shelves.

Each number specification can also specify a starting number and some
tags and attribute. If tags/attributes are specified, they will be
used to sort the boxes before decidind which are the n first. If a
"shelf" tag is specified the value of the tag will be used to group
boxes instead of using the shelfname. Example

::

   TShirt^^^2:2 => The second box containg a t-shirt
   TShirt^^^1:2 => The first, second box containg a t-shirt
   TShirt^^^[price]1 => The box with the lowest price
   TShirt^^^-[price]5 => the five box with hight price
   TShirt^^^[price]{coordinate} => sort boxes by price tag and coordinate attributes
::rST -}
parseBoxNumberSelector :: Bool -> Text -> BoxNumberSelector
parseBoxNumberSelector _ "" = BoxNumberSelector NoLimit NoLimit NoLimit
parseBoxNumberSelector defUseBase s = case P.parse parser (unpack s) s of 
  Left err -> error (show err)
  Right expr -> expr
  where parser = do
          limits <- parseLimit defUseBase `P.sepBy` P.char '^'
          case limits of 
               (_:_:_:_:_) -> fail "Too many limits in"
               _ -> let (content: shelves: total:_) =  limits ++ List.cycle [NoLimit]
                    in return $ BoxNumberSelector content shelves total
                

-- | Parsel [[tag]|{attribue}][min:][max]
parseLimit :: Bool -> MParser Limit      
parseLimit defUseBase = do
  useBase <- P.option defUseBase $ (P.char '=' >> return False) <|> (P.char '@' >> return True)
  keys <- P.many do
      order <- P.option NormalOrder (P.char '-' >> return ReverseOrder)
      key <- (parseTag <|> parseAttribute)
      return $ (key, order)
  minM <- P.optional $ P.decimal
  maxMM <- P.optional $ P.char ':' >> P.optional P.decimal
  let (start, end) = case (minM , maxMM ) of
        -- :max or :
        -- (Nothing, Just maxm) -> (Nothing, maxm)
        -- max
        (Just min_, Nothing) -> (Nothing, Just min_)
        -- min:
        (Just min_, Just Nothing) -> (Just min_, Nothing)
        (minm, Just maxm) -> (minm, maxm)
        (Nothing, Nothing) -> (Nothing, Nothing)
  return $  case Limit start end keys useBase of
                 -- NoLimit -> NoLimit -- don't use defUseBase if not needed. Needed to make test easier
                                    -- so than NoLimit {liUseBase = False} = NoLimit
                 limit -> limit
  where parseTag = OrdTag  <$> between '[' ']'
        parseAttribute = OrdAttribute  <$> between '{' '}'

between :: Char -> Char -> MParser Text
between open close = P.char open >> (P.takeWhileP Nothing (/= close)) <* P.char close

  
applyPattern :: MatchPattern -> Text -> Bool
applyPattern pat value = case pat of
  MatchAnything -> True
  MatchFull value0 -> value == value0
  MatchGlob glob -> Glob.match glob (unpack value)
-- * Misc
matchName :: Text -> NameSelector s
matchName name = NameMatches [MatchFull name]


specials = "*?[]{}<>" :: String
isGlob :: Text -> Bool
isGlob s = case break (`List.elem` specials) s of
  (_, uncons -> Just _) -> True
  _ -> False
  
-- ** Print
printSelector :: Selector s -> Text 
printSelector Selector{..} = printNameSelector nameSelector <> mconcat (map (("#" <>) . printTagSelector) tagSelectors)

printNameSelector :: NameSelector s -> Text
printNameSelector (NameMatches pats) = intercalate "|" (map printPattern pats)
printNameSelector (NameDoesNotMatch pats) = "!" <> intercalate "|" (map printPattern pats)

printTagSelector :: TagSelector s -> Text
printTagSelector = \case
  TagHasKey key -> printPattern key
  TagHasNotKey key -> "-" <> printPattern key
  TagIsKey key -> printPattern key
  TagIsKeyAndValues key vals -> printPattern key <> "=" <> printVals vals
  TagHasKeyAndValues key vals -> printPattern key <> "=+" <> printVals vals
  TagHasValues vals -> "=" <> printVals vals
  TagHasNotValues vals -> "=-" <> printVals vals
  TagHasKeyAndNotValues key vals -> printPattern key <> "=-" <> printVals vals
  where printVals = intercalate ";" . map printVPattern


printPattern :: MatchPattern -> Text
printPattern (MatchFull pat) = pat
printPattern (MatchAnything) = ""
printPattern (MatchGlob pat) = pack $ Glob.decompile pat

printVPattern :: ValuePattern -> Text
printVPattern (VMatch pat) = printPattern pat
printVPattern (VTag tag) = "$[" <> tag <> "]"

printNumberSelector :: BoxNumberSelector -> Text
printNumberSelector (BoxNumberSelector NoLimit NoLimit NoLimit) = ""
printNumberSelector  BoxNumberSelector{..} = mconcat [ "^" <> printLimit lim
                                                     | lim <- [nsPerContent, nsPerShelf, nsTotal ]
                                                     ]


printLimit :: Limit -> Text
printLimit Limit{..} = key <> lim where
  key = mconcat [ useBase <> rev <> case k of
                   OrdTag t -> "[" <> t <> "]"
                   OrdAttribute a -> "{" <> a <> "}"
                | (k, reverse) <- liOrderingKey
                , let rev = if reverse == ReverseOrder then "-" else ""
                , let useBase = if liUseBase then "" else "="
                ]
  lim = case (liStart, liEnd) of
         (Just s, Just e) -> tshow s <> ":" <> tshow e
         (Just s, Nothing) -> tshow s
         (Nothing, Just e) -> ":" <> tshow e
         (Nothing, Nothing) -> ""

printBoxSelector :: BoxSelector -> Text
printBoxSelector BoxSelector{..} = let
     shelf = case shelfSelectors of
                SelectAnything -> ""
                s -> "/" <> printSelector s
     in printSelector boxSelectors <> shelf <> printNumberSelector numberSelector

printShelfSelector :: ShelfSelector -> Text
printShelfSelector ShelfSelector{..} = 
   case sBoxSelectors  of
       SelectAnything -> printSelector sShelfSelectors
       _ -> printSelector sBoxSelectors <> "/" <> printSelector sShelfSelectors

-- * Util
-- split on given char unless the char is escaped with \
-- split on # a#b -> [a, b] a\#b a#b
splitOnNonEscaped :: Text -> Text -> [Text]
splitOnNonEscaped _ txt | null txt = [""]
splitOnNonEscaped needle txt = go txt
   where go txt | null txt = []
         go txt =
            case breakOn needle txt of
              (before, rest) | null rest -> [before] -- no more separators
                             | Just beforeWithoutEspace <- stripSuffix "\\" before ->
                                  case go (drop 1 rest) of
                                        [] -> [ beforeWithoutEspace <> needle ]
                                        (x:xs) -> (beforeWithoutEspace <> needle <> x) : xs
                             | otherwise -> before : splitOnNonEscaped needle (drop 1 rest)


                          
