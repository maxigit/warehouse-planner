{-# LANGUAGE OverloadedStrings #-}
module WarehousePlanner.WPL.Parser
(
wplParser
, o1, o2, o3
)
where 

import ClassyPrelude hiding(some, many, try, (<|))
import WarehousePlanner.Selector (MParser, parseSelector, parseShelfSelector, parseBoxSelectorWithDef, splitOnNonEscaped)
import WarehousePlanner.WPL.Types
import Text.Megaparsec as P hiding((<?>))
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug qualified as P
import Text.Megaparsec.Char
import Data.Char
import Data.List.NonEmpty(nonEmpty, NonEmpty(..), some1)
import WarehousePlanner.Type
import Data.Foldable qualified as F
import Control.Monad(fail)
import WarehousePlanner.Base
import Data.Monoid (Last(..))
import Data.Map qualified as Map
import WarehousePlanner.Expr
import Data.Text (splitOn)
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

-- import WarehousePlanner.WPL.PrettyPrint2


-- dbg _ = id
dbg :: Show a => String -> MParser a -> MParser a
-- dbg = P.dbg
dbg _ = id

-- avoid redundant import
_dbg :: Show a => String -> MParser a -> MParser a
_dbg = P.dbg
(<?>):: Show a => MParser a -> String -> MParser a
p <?> lbl = dbg lbl $ label lbl p
dlabel lbl p = p <?> lbl

wplParser :: MParser [Statement]
-- wplParser = sepBy p (lexeme ",") <* spaces  <* eof
wplParser = (optional "version:2" *> spaces) *> many p <* spaces  <* eof

class Parsable a where
   p :: MParser a

-- * Whitespaces & Co
-- starting with -- but we need a space so that --| is not a comment
lineComment :: MParser ()
lineComment = L.skipLineComment "---"
            <|> L.skipLineComment "-- "
            <|> try (void "--" <> lookAhead (void eol <|> eof)) -- finishinig a line


blockComment :: MParser ()
blockComment = L.skipBlockComment "{-" "-}"

-- | Spaces with new lines
spaces :: MParser ()
spaces = L.space space1
                 lineComment
                 blockComment

lexeme :: MParser a -> MParser a
lexeme = L.lexeme spaces
lexeme1 p = do
   r <- p
   lookAhead $ asum [space1, lineComment, blockComment, eof, void "}", void ")" ]
   spaces
   return r
   
instance Parsable Statement where
         p = asum [  do
                      lexeme ";"
                      a <- lexeme p
                      return $ PassThrought a
                  , do -- if
                      lexeme1 "if"
                      cond <- p
                      then_ <- subStatement  <?> "then"
                      lexeme1 "else"
                      else_ <- subStatement <?> "else"
                      return $ If cond then_ (Just else_)
                  , do -- unless
                      lexeme1 "unless"
                      cond <- p
                      then_ <- subStatement  <?> "else"
                      return $ If (CondNot $ cond)  then_ Nothing
                  , do -- when
                      lexeme1 "when"
                      cond <- p
                      then_ <- subStatement  <?> "then"
                      return $ If cond then_ Nothing
                  , do
                     a <- lexeme atom
                     fm <- optional $ try followUp
                     return case fm of 
                       Nothing -> a
                       Just f -> f a
                  ]
             where atom  = asum [ 
                           try do
                             (o:os) <- between (lexeme "(") (lexeme ")") (sepBy1 (lexeme p) (lexeme $ ","))
                             return case os of
                                [] -> o
                                _ -> Ors $ o :| os
                          , withStatement "foreach:shelf" (lexeme1 "foreach:shelf" $> ForeachShelf)
                          , withStatement "foreach:box" do 
                                  lexeme1 "foreach:box" 
                                  boxes <- lexeme p
                                  return $ ForeachBox boxes
                          , do
                              lexeme "foreach:do"
                              st <- subStatement
                              (o:os) <- between (lexeme "(") (lexeme ")") (sepBy1 p (lexeme $ ","))
                              return $ ForeachDo st (o :| os)
                          , do
                             (c:cs) <- between (lexeme "/[") (lexeme "]") (sepBy1 p (lexeme $ "|"))
                             return $ ShelfCases $ fmap (\st -> ShelfCase st Nothing) (c:|cs)
                          , do
                             (c:cs) <- between (lexeme "[") (lexeme "]") (sepBy1 p (lexeme $ "|"))
                             return $ Cases $ fmap (\st -> Case st Nothing) (c:|cs)
                          , withStatement "trace:pretty" do
                              lexeme1 ("trace:pretty" <|> "t:p")
                              descM <- lexeme $ o1  ["m"]
                              return $ PrettyPrint (fromMaybe "T:P" descM)
                          , Action <$> p 
                          ]
                   followUp = asum [ do 
                                      pos <- getSourcePos
                                      if (unPos (sourceColumn pos) <= 1)
                                      then do
                                         spaces 
                                         return id
                                      else do
                                           b <- p
                                           return \a -> Then a b

                                   ]
                 
subStatement :: MParser Statement
subStatement = lexeme "{" *> lexeme p <* lexeme "}"
               <|> lexeme "&" *> lexeme p

withStatement :: String -> MParser (Statement -> a) -> MParser a
withStatement name parser = do
   constructor <- parser
   stmt <- subStatement <?> name
   return  $ constructor stmt

instance Parsable Command where
  p = dlabel "command" $ asum
        [ do -- Move
            exitMode <- (lexeme1 "to^" $> ExitOnTop) <|> (lexeme1 "to>" $> ExitLeft) 
            (sourceM, pmode, orules) <- o3 boxesK pmodeK oRulesK
            shelf <-  p
            return $ Move sourceM pmode (fromMaybe [] orules) $ singleton (exitMode, shelf)
        , do -- Move, the exitMode is after the options
            lexeme "to " -- avoid collision with toggle
            (sourceM, pmode, orules) <- o3 boxesK pmodeK oRulesK
            exit'shelves <- some1 $ liftA2 (,) (lexeme p) p
            return $ Move sourceM pmode (fromMaybe [] orules) exit'shelves
        , do -- Tag
           void $ optional $ lexeme "tag"
           "#"
           tags <- lexeme1 $ str -- takeWhileP (Just "tags") (not . isSpace)
           let tagOps = parseTagOperations tags
           boxesM <- o1 boxesK
           case boxesM of
              Just boxes -> do
                         sub <- subStatement
                         return $ TagFor boxes tagOps sub
              Nothing -> do
                      subM <- optional $ subStatement <?> "sub"
                      case subM of
                           Nothing -> return $ Tag tagOps
                           Just sub -> do -- temporary tags
                                    return $ TagFor (CSelector selectAllBoxes) tagOps sub
        , do -- Toggle
           void $ optional $ lexeme "toggle"
           "#"
           tagOps <- lexeme1 $ str -- takeWhileP (Just "tags") (not . isSpace)
           return $ ToggleTags (parseTagOperations tagOps)
        , do -- TagShelves
           lexeme ("shelf:tag"  <|> "shelves:tag" <|> "stag")
           "#"
           tagOps <- lexeme $ str -- takeWhileP (Just "tags") (not . isSpace)
           return $ TagShelves (parseTagOperations tagOps)
        , do -- SelectShelves
           lexeme "/"
           sel <- p
           return $ SelectShelves sel
        , do -- SelectBoxes
           sel <- p
           return $ SelectBoxes sel
        , do -- Box ranges
             boundary <- asum $ [ lexeme1 "from" $> From
                                , lexeme1 "upto" $> Upto
                                , lexeme1 "before" $> Before
                                , lexeme1 "after" $> After
                                ]
             selector <- label "boundary selector" p
             return $ SelectBoxRanges boundary selector
        , do -- Tam
           lexeme1 "tam"
           orules <- o1 oRulesK
           tagLoc <- lexeme1 $ str <?> "loc#tag" --  takeWhile1P (Just "loc#tag") (not . isSpace)
           return $ TagAndMove tagLoc (fromMaybe [] orules)
        , lexeme1 "delete" >> return Delete
        , do
            lexeme ("pmode=" <|> "p=")
            pmode <- p
            return $ SetPartitionMode pmode
        , do
            cons <- lexeme $ asum [ SetOrientationStrategies <$ ("orules=" <|> "o=") 
                                  , AddOrientationStrategies <$ ("orules+=" <|> "o+=")
                                  ]
            os <- p
            selM <- o1 forK  
            return $ cons selM os

        , do -- TraceCount
            (lexeme1 $ "trace:count" <|> "t:c")
            descM <- o1  msgK
            return $ TraceCount (fromMaybe "T:C" descM)
        , do -- TraceBoxes
           lexeme1 $ asum ["trace:boxes", "trace:box", "t:b"]
           (descM, propM) <- o2 msgK propK
           return $ TraceBoxes (fromMaybe "T:B" descM) propM
        , do -- TraceShelves
            (lexeme1 $ "trace:shelves" <|> "t:s")
            descM <- o1  msgK
            return $ TraceShelves (fromMaybe "T:S" descM)
        , do -- TraceOrientations
            (lexeme1 $ "trace:orientation" <|> "t:o")
            descM <- o1  msgK
            return $ TraceOrientations (fromMaybe "T:O" descM)
        , do -- Set NoEmp
           lexeme1 $ "empty:boxes=yes"
           return $ SetNoEmptyBoxes False
        , do -- Set NoEmp
           lexeme1 $ "empty:boxes=no"
           return $ SetNoEmptyBoxes True
        , do
          lexeme1 "empty:shelves=no"
          return $ SetNoEmptyShelves True
        , do
            lexeme1 "empty:shelves=yes"
            return $ SetNoEmptyShelves False
        , do -- assert
          b <- (lexeme ("assert:noboxes" <|> "a:nob")  $> True)
               <|> (lexeme ("assert:boxes"  <|> "a:b") $> False)  
          descM <- o1 msgK
          return $ AssertBoxes b (fromMaybe (if b then "A:Boxes" else "A:Boxes") descM)
        , do -- assert
          b <- (lexeme ("assert:noshelves" <|> "a:nos")  $> True)
               <|> (lexeme ("assert:shelves"  <|> "a:s") $> False)  
          descM <- o1 msgK
          return $ AssertShelves b (fromMaybe (if b then "A:Shelves" else "A:Shelves") descM)
        , withStatement "shelf:full" do
                        lexeme1 $ "shelf:full"
                        sel <- p
                        return $ ResizeShelfFull sel
        , withStatement "shelf:resize" do
                        lexeme1 $ "shelf:resize"
                        sel <- p
                        (lsm, wsm, hsm) <- o3 ["l", "length"] ["w", "width"] ["h", "height"]
                        let defRef = parseExpr "{}"
                        let [ls, ws, hm] = map (fromMaybe defRef ) [lsm, wsm, hsm]
                        return $ ResizeShelf sel ls ws hm
        , withStatement "box:resize" do
                        mode <- asum [ lexeme1 "bsize:max"  $> MaxDimension
                                     , lexeme1 "bsize:min" $> MinDimension
                                     , lexeme1 "bsize:first" $> FirstDimension
                                     ]
                        selM <- o1 boxesK
                        return $ ResizeBox mode (fromMaybe (CSelector selectAllBoxes) selM)
        , withStatement "shelf:split" do
                        lexeme1 $ "shelf:split"
                        sel <- p
                        boxM <- o1 boxesK
                        (lsm, wsm, hsm) <- o3 ["l", "length"] ["w", "width"] ["h", "height"]
                        let [ls, ws, hm] = map (fromMaybe [] ) [lsm, wsm, hsm]
                        return $ SplitShelf sel boxM ls ws hm
        , do
                        lexeme1 "swap"
                        (debugPrefixM, stickym) <- o2 ["debug", "d"] ["sticky", "s"]
                        selector <- p
                        let stickies = maybe [] (splitOnNonEscaped "#") stickym
                        return $ SwapBoxes selector debugPrefixM stickies
        , withStatement "fill" do
            lexeme1 "fill"
            (posM, locM) <- lexeme $ o2 ["pos", "p"]  ["loc", "l"]
            return $ FillShelves locM posM

        ]
        where msgK = [ "msg",  "message", "m"]
              boxesK =  [ "boxes", "box", "b"]
              pmodeK = [ "pmode" , "p"]
              oRulesK = [ "orules" , "o"]
              propK = [ "prop", "property", "pr" ]
              -- shelvesK = ["shelf", "shelves", "s"]
              forK = ["for", "f"]
              
-- * Selector
instance Parsable BoxSelector where
   p = dlabel "box selector" $ do
                guardLower
                t <- lexeme1 (takeWhile1P (Just "selector") isSelector)
                return $ parseBoxSelectorWithDef False t
    where guardLower :: MParser ()
          guardLower = label "Escape lower case with ?" $ void $ (char '?') <|> lookAhead upperChar
         

instance Parsable ShelfSelector where
  p = dlabel "shelf selector" $ asum
            [ do
                "?"
                t <- getText
                return $ parseShelfSelector t
            ,
              do
                t <- getText
                return $ ShelfSelector SelectAnything (parseSelector t)
            ]
      where isShelfSelector c = isSelector c && (c `notElem` ("<^>" :: String)) 
            --                                                 ^^^
            --                                                 ||+--- exitmode left
            --                                                 |+---- exitmode top
            --                                                 +----- start of numeric range
            --                                                        which end with >
            getText = do
                      ts <- lexeme $ some1 (range <|> takeWhile1P (Just "selector") isShelfSelector)
                      return $ sconcat ts
            range = do
                     char '<'
                     r <- takeWhile1P (Just "range") (/='>')
                     char '>'
                     return $ "<" <> r <> ">"


       
instance Parsable (Selector s) where
   p = do 
        t <- lexeme $ takeWhile1P (Just "selector") isSelector
        return $ parseSelector t
instance Parsable (CSelector BoxSelector) where
  p  =  label "cbox" $ asum
    [  lexeme1 "in:shelves" >> return CUseContext
    ,  lexeme1 "in" >> do 
                          shelves <- p
                          return  $ CSelector (selectAllBoxes { shelfSelectors = shelves} )
    , cselector
    ]


instance Parsable (CSelector ShelfSelector) where
  p = label "cshelf" $ asum
     [ lexeme1 "with:boxes" >> return CUseContext
     , lexeme1 "with" >> do 
                            BoxSelector b s _  <- p
                            return $ CSelector (ShelfSelector b s)
     , cselector
     ]
isSelector :: Char -> Bool
isSelector c = not $ isSpace c || c == ')'


cselector :: Parsable s => MParser (CSelector s)
cselector = try $  do
        gs <- many go
        selectorm <- case gs of
          [] -> Just <$> sel
          _ -> lexeme $ optional sel
        case gs <> toList selectorm of 
           [] -> fail "some returning []"
           [one] -> return one
           cs -> return $ F.foldr1 CSelectorAnd cs
  where go = asum [swapContext, root, parent, useContext, cstatement] where
        swapContext = do
            (string "-~")
            return SwapContext
        sel = CSelector <$> p
        parent = (char '~') >> return Parent
        root = (string ".~") >> return Root
        useContext = "<useContext>" >> return CUseContext
        -- ^^^^^^ not to be used by human. here for quickcheck : to be able to parse
        -- pretty print.
        cstatement = lexeme "(" *> (CStatement <$> lexeme p <?> "cstatement") <* ")"
                     --                                           ^^^^^
                     --                                            |
                     --  No lexeme in here. Chain of selectors ----+
                     --  can not be separated by space

instance Parsable [OrientationStrategy] where
  p = ( lexeme1 "<empty>" >> return []) <|> do
    rule <- lexeme $ takeWhile1P (Just "orientation rules") (not . isSpace)
    let rules = splitOn "," rule
    mconcat <$> mapM go rules
    where go rule = case parseOrientationRule [tiltedForward, tiltedFR] rule of
                         [] | not (null rule) -> fail "not an rule"
                         rules -> return rules
          
instance Parsable PartitionMode where
  p = partitionModeParser

instance Parsable ExitMode where
  p = ("^" $> ExitOnTop) <|> (">" $> ExitLeft )
-- * Option utilites

   
instance Parsable Text
  where p = str
  
instance Parsable (Expr Text) where
   p = lexeme exprParser
   
instance Parsable [Expr Text] where
  p = lexeme $ exprParser `P.sepBy1` lexeme ":"

str :: MParser Text
str = asum [ pack <$> (char '"' >> manyTill L.charLiteral (char '"' ))
           , do "{" ; fail "string are not allowed to start with {"
           , takeWhileP (Just "string") (\c -> not $ isSpace c || c == ')')
           ]
    
o :: Show a => Parsable a => [Text] -> MParser a
o [] = o [""]
o keys@(k:_) = do
   void $ asum [ string (key <> ":") | key <- toList keys ]
   p <?> unpack k
    
-- | Add "" to a list of keys
o0 :: Show a => Parsable a => [Text] -> MParser a
o0 keys = o (keys ++ [""])

combine2 :: Show a => Show b => MParser a -> MParser b -> MParser (Maybe a, Maybe b)
combine2 pa pb = do
     es <- some $ lexeme (((,Nothing) . Just)  <$> try pa <|> ((Nothing,) . Just) <$> pb )
     return case nonEmpty $ map (bimap Last Last) es of 
        Nothing -> (Nothing, Nothing)
        Just ne -> bimap (getLast) (getLast) $ sconcat ne
     -- aOrbM <- optional $ Left <$> pa <|> Right <$> pb
     -- case aOrbM of
     --    Nothing -> return (Nothing, Nothing)
     --    Just (Left a) -> do
     --              bm <- optional pb
     --              return (Just a, bm)
     --    Just (Right b) ->  do
     --              am <- optional pa
     --              return (am, Just b)


o1 :: Show a => Parsable a => [Text] -> MParser (Maybe a)
o1 = optional . o0

o2 :: Show a => Show b => Parsable a => Parsable b => [Text] -> [Text] -> MParser (Maybe a, Maybe b)
o2 a b = do
    checkForDuplicate [a, b ]
    fromMaybe (Nothing, Nothing) <$> ( optional $ combine2 (o0 a) (o b) )

o3 :: (Parsable a, Parsable b, Parsable c, Show a, Show b, Show c) => [Text] -> [Text] -> [Text] -> MParser (Maybe a, Maybe b, Maybe c)
o3 ka kb kc = do
    checkForDuplicate [ka, kb, kc]
    (abm, cm) <- fromMaybe (Nothing, Nothing) <$> ( optional $ combine2 (combine2 (o0 ka) (o kb)) (o kc) )
    let (am, bm) = fromMaybe (Nothing, Nothing) abm
    return (am, bm, cm)

-- | Check that all set of key don't have duplicate
-- raise a parsing in error if needed
checkForDuplicate :: [[Text]] -> MParser ()
checkForDuplicate keyss = let
    keysWithCount = Map.fromListWith (+) $ map (,1 :: Int) $ concat keyss
    in case Map.filter (>1) keysWithCount  of
        dupMap | null dupMap  -> return ()
        dupMap ->  let errors = [ ErrorFail $ "'" <> unpack k <> ": used for different options"
                                | k <- keys dupMap
                                ]
                   in P.registerFancyFailure $ setFromList errors


instance Parsable Condition where
  p = makeExprParser term table <?> "conditions" where
      term = between (lexeme "(") (lexeme ")") p <|> asum [ "/" *> do CondShelf <$> p
                                                          , CondBox <$> p
                                                          ]
      table = [ [ prefix "!" CondNot ]
              , [ binary (lexeme "&&") CondAnd ]
              , [ binary (lexeme "||") CondOr ]
              ]
      prefix symbol f = Prefix (f <$ symbol)
      binary op f = InfixL (f <$ op)

  -- p = asum [ "!" *> do CondNot <$> p 
  --          ]

