module WarehousePlanner.Selector
( applyNameSelector
, applyTagSelectors
, matchName
, parseBoxSelector
, parseShelfSelector
, parseSelector
, parseNameSelector
, parseMatchPattern
, parseTagSelector
, printSelector, printBoxSelector, printShelfSelector
) where
import ClassyPrelude hiding (uncons, stripPrefix, unzip)
import WarehousePlanner.Type
import System.FilePath.Glob qualified as Glob
import Text.Parsec qualified as P
import Text.Parsec.Text qualified as P
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Map.Lazy qualified as Map
import Data.Text (splitOn, uncons, stripPrefix)
import Control.Monad.Fail 

-- * Selectors 
-- ** Applying 
-- | The phantom type guarantie that we are selecting the correct item
applyNameSelector :: NameSelector a -> (a -> Text) -> a -> Bool
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
  MatchFull key | Just values <- lookup key tags -> matchesAllAndAll valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndAll valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasKeyAndValues pat valuePats) tags = case pat of
  MatchFull key | Just values <- lookup key tags -> matchesAllAndSome valuePats  values
  MatchAnything -> True
  MatchGlob glob ->  case filter (Glob.match glob . unpack) (keys tags) of
    [key] | Just values <- lookup key tags -> matchesAllAndSome valuePats values
    _ -> False
  _ -> False
applyTagSelector (TagHasValues valuePat) tags = let
  tagValues = mconcat (Map.elems tags)
  in matchesAllAndSome valuePat tagValues
applyTagSelector (TagHasNotValues valuePat) tags = not $ applyTagSelector (TagHasValues valuePat) tags
applyTagSelector (TagHasKeyAndNotValues key valuePat) tags = not (applyTagSelector (TagHasKeyAndValues key valuePat) tags)

applyTagSelectors :: Show s => [TagSelector s] -> (s -> Tags) -> s -> Bool
applyTagSelectors [] _ _ = True
applyTagSelectors selectors tags o =  all (flip applyTagSelector (tags o)) selectors

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

-- ** Parsing 
-- | split on |
parseSelector :: Text -> Selector a
parseSelector s = case splitOn "#" s of
  [] -> Selector(NameMatches []) []
  (name:tags) -> Selector (parseNameSelector name) (mapMaybe parseTagSelector tags)

parseNameSelector :: Text -> NameSelector a
parseNameSelector selector = let
  (constr, pat) = case uncons selector of
       Just ('!', sel) -> (,) NameDoesNotMatch sel
       _ ->  (,) NameMatches selector
  in constr $ map parseMatchPattern (splitOn "|" pat)

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
  where mkValues = map parseMatchPattern . fromList . splitOn ";"
  
parseMatchPattern :: Text -> MatchPattern
parseMatchPattern "" = MatchAnything
parseMatchPattern pat | isGlob pat= MatchGlob (Glob.compile $ unpack pat)
parseMatchPattern pat = MatchFull pat
  

parseBoxSelector :: Text -> BoxSelector s
parseBoxSelector selector = let
  (box'location, drop 1 ->numbers) = break (=='^') selector
  (box, drop 1 -> location) = break (=='/') box'location
  in BoxSelector (parseSelector box)
              (parseSelector location)
              (parseBoxNumberSelector numbers)

parseShelfSelector :: Text -> ShelfSelector s
parseShelfSelector selector = let
  BoxSelector boxSel shelfSel _ = parseBoxSelector selector
  in ShelfSelector boxSel shelfSel

parseBoxNumberSelector :: Text -> BoxNumberSelector
parseBoxNumberSelector "" = BoxNumberSelector Nothing Nothing Nothing
parseBoxNumberSelector s = case P.parse parser (unpack s) s of 
  Left err -> error (show err)
  Right expr -> expr
  where parser = do
          limits <- P.optionMaybe parseLimit `P.sepBy` P.char '^'
          case limits of 
               (_:_:_:_:_) -> fail "Too many limits in"
               _ -> let (content: shelves: total:_) =  limits ++ List.cycle [Nothing]
                    in return $ BoxNumberSelector content shelves total
                
-- | Parsel [[tag]|{attribue}][min:][max]
parseLimit :: P.Parser Limit      
parseLimit = do
  reverse <- P.option False (P.char '-' >> return True)
  keys <- P.many (parseTag <|> parseAttribute)
  minM <- P.optionMaybe $ P.many1 P.digit
  maxMM <- P.optionMaybe $ P.char ':' >> P.optionMaybe (P.many1 P.digit)
  let (start, end) = case (minM >>= readMay,  fmap (>>= readMay) maxMM ) of
        -- :max or :
        -- (Nothing, Just maxm) -> (Nothing, maxm)
        -- max
        (Just min_, Nothing) -> (Nothing, Just min_)
        -- min:
        (Just min_, Just Nothing) -> (Just min_, Nothing)
        (minm, Just maxm) -> (minm, maxm)
        (Nothing, Nothing) -> (Nothing, Nothing)
  return $  Limit start end keys reverse
  where parseTag = OrdTag . pack <$> do P.char '[' >> P.many1 (P.noneOf "]") <* P.char ']'
        parseAttribute = OrdAttribute . pack <$> do P.char '{' >> P.many1 (P.noneOf "}") <* P.char '}'

  
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
  where printVals = intercalate ";" . map printPattern


printPattern :: MatchPattern -> Text
printPattern (MatchFull pat) = pat
printPattern (MatchAnything) = ""
printPattern (MatchGlob pat) = pack $ Glob.decompile pat


printNumberSelector :: BoxNumberSelector -> Text
printNumberSelector (BoxNumberSelector Nothing Nothing Nothing) = ""
printNumberSelector  BoxNumberSelector{..} = mconcat [ "^" <> maybe "" printLimit lim
                                                     | lim <- [nsPerContent, nsPerShelf, nsTotal ]
                                                     ]


printLimit :: Limit -> Text
printLimit Limit{..} = rev <> key <> lim where
  rev = if liReverse then "-" else ""
  key = mconcat [ case k of
                   OrdTag t -> "[" <> t <> "]"
                   OrdAttribute a -> "{" <> a <> "}"
                | k <- liOrderingKey
                ]
  lim = case (liStart, liEnd) of
         (Just s, Just e) -> tshow s <> ":" <> tshow e
         (Just s, Nothing) -> tshow s
         (Nothing, Just e) -> ":" <> tshow e
         (Nothing, Nothing) -> ""

printBoxSelector :: BoxSelector s -> Text
printBoxSelector BoxSelector{..} = let
     shelf = case shelfSelectors of
                SelectAnything -> ""
                s -> "/" <> printSelector s
     in printSelector boxSelectors <> shelf <> printNumberSelector numberSelector

printShelfSelector :: ShelfSelector s -> Text
printShelfSelector ShelfSelector{..} = 
   case sBoxSelectors  of
       SelectAnything -> printSelector sShelfSelectors
       _ -> printSelector sBoxSelectors <> "/" <> printSelector sShelfSelectors
