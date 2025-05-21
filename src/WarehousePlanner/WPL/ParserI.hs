module WarehousePlanner.WPL.ParserI
( wplParserI
)
where

import ClassyPrelude hiding(some, many, try)
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector
import WarehousePlanner.Type
import WarehousePlanner.Base
import WarehousePlanner.Expr
-- import WarehousePlanner.Type
import Text.Megaparsec as P hiding((<?>))
import Text.Megaparsec.Debug qualified as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad(fail)
import Data.Foldable qualified as F

-- dbg _ = id
dbg :: Show a => String -> MParser a -> MParser a
-- dbg = P.dbg
dbg _ = id

-- avoid redundant import
_dbg :: Show a => String -> MParser a -> MParser a
_dbg = P.dbg
(<?>):: Show a => MParser a -> String -> MParser a
p <?> lbl = dbg lbl $ label lbl p

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


-- | Spaces without new lines
hspaces :: MParser ()
hspaces = L.space hspace1
                 lineComment
                 empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme hspaces
lexeme1 p = do
   r <- p
   lookAhead $ asum [space1, lineComment, blockComment, eof ]
   hspaces
   return r

newLine = hspaces >> (void eol <|> eof)
wplParserI ::  MParser [Statement]
wplParserI = (optional "version:1") *> (some $ L.nonIndented spaces $ statement) <*  spaces <* eof


statement :: MParser Statement
statement = asum
   [ indentedBlock <?> "statement:case block"
   , thenMulti <?> "statement:then multi"
   ]  <?> "statement:one"


caseBlock :: MParser Statement
caseBlock = do
  blockOf "case" caseLine Cases

shelfCaseBlock :: MParser Statement
shelfCaseBlock = do
  blockOf "shelfCase" shelfCaseLine ShelfCases

thenBlock :: MParser Statement
thenBlock = do
  blockOf "thens" line mkThen 
  where line = do
          lexeme "&" <?> "& line"
          indentedBlock <|> thenMulti
        mkThen = F.foldr1 Then 
        --       ^^^^^^^^
        --       [a, b, c] -> a Then (b Then c)
        --       
passThrougBlock = do
   blockOf "passthrough" line (PassThrought . mkOrs)
   where line = do
          lexeme ";" <?> "passthrough"
          indentedBlock <|> thenMulti

indentedBlock = caseBlock <|> shelfCaseBlock <|> thenBlock <|> passThrougBlock

-- | Statements with same indentation
blockOf :: String -> MParser a -> (NonEmpty a -> b) -> MParser b
blockOf name p mk = do
  -- consume possible space to be sure to start at the beginning of
  -- block to get the indentation correct
  iLvl <- L.indentLevel
  let iguard = try $ L.indentGuard hspaces EQ iLvl <?> (name <> ":blockGuard " <> show iLvl)
  cs <- some $ iguard >> p
  case cs of
     [] -> fail "some returning []"
     c:cs -> return $ mk (c :| cs)

thenLine :: MParser Statement
thenLine =do
  beforeLvl <- L.indentLevel
  a <- atom <?> "line:atom"
  -- if the atom consume a new line, then there is nothing else to to parse
  afterLvl <- L.indentLevel
  if afterLvl <= beforeLvl
  then return a
  else do 
     thenm <- optional $ try $ asum [  indentedBlock <?> "case in line"
                             , thenLine <?> "next in line"
                             ]
     case thenm of
        Nothing -> return a <* (newLine <?> "end of thenLine")
        Just then_ -> return $ a `Then` then_

thenMulti :: MParser Statement
thenMulti = do
  iLvl <- L.indentLevel
  line <- thenLine <?> "thenMulti:line"
  spaces
  -- get different blocks of decreasing indentation
  childrenm <- many $ (L.indentGuard spaces GT iLvl <?> ("thenMulti:guard " <> show iLvl))
                        >> (orBlock ("thenMulti" <> show iLvl)  <?> "thenMulti:children")
  return case childrenm of
         [] -> line
         ors -> F.foldl1 Then (line :| ors)
         --     ^^^^^^^^
         --       [a, b, c] ->  (a Then b) Then c

foreachS :: MParser Statement
foreachS = withStatement "foreach:shelf" do
  "foreach:shelf"
  return $ ForeachShelf 
  
foreachB :: MParser Statement
foreachB = withStatement "foreach:box" do
    lexeme "foreach:box"
    selector <- boxSelector
    return $ ForeachBox selector

foreachDo :: MParser Statement
foreachDo = withStatement "foreach:do" do
    lexeme "foreach:do"
    return \ors -> case ors of
                          Ors (action :| (x:xs)) -> ForeachDo action (x :| xs)
                          statement -> statement
                          
prettyPrint :: MParser Statement
prettyPrint =
    withStatement "trace:pretty"
                  do
                    lexeme "trace:pretty"
                    title <- lexeme1 $ takeWhile1P (Just "title") (not . isSpace)
                    return $ PrettyPrint title
    <|> withStatement "trace:pretty"
                  do
                    lexeme "t:p"
                    return $ PrettyPrint "PRETTY"

  

orBlock name = do
  blockOf (name <> ":or") ((indentedBlock <?> "indentedBlock")
          <|>
          (thenMulti <?> "line block")
          ) mkOrs

mkOrs :: NonEmpty Statement -> Statement
mkOrs ors = case ors of
   o :| [] -> o
   _ -> Ors ors



caseLine :: MParser Case
caseLine = do
    passthrough <- (lexeme "||" >> return False) <|> (lexeme "|" >> return True) <?> "start caseline"
    let withMulti = \case  
                             Then a b | passthrough  -> Case a (Just b)
                             c -> Case c Nothing
    fmap (flip Case Nothing) indentedBlock <|> fmap withMulti (orBlock "case")

shelfCaseLine :: MParser ShelfCase
shelfCaseLine = double <|> simple 
    where double = do
                 lexeme "//" <?> "double shelfcase"
                 shelves <- some $ lexeme shelfSelector
                 newLine
                 flip ShelfCase Nothing <$> case shelves of
                    [] -> fail "some returning []"
                    [one] -> return $ select one
                    (c:cs) -> return . F.foldr1 Then $ fmap select (c :| cs)
          select = Action . SelectShelves 
          simple = do 
                try $ lexeme "/ " <?> "simple shelfcase"
                shelf <- shelfSelector
                thenm <- (Just <$> thenMulti) <|> (newLine  >> return Nothing)
                return $ ShelfCase (select shelf) thenm

 

atom :: MParser Statement
atom = -- (PassThrought <$> (lexeme ";" *> statement ))
       -- ("("  *> statement <* ")")
       foreachS
       <|> foreachB
       <|> foreachDo
       <|> prettyPrint
       <|> (notFollowedBy "|" >> Action <$> command )

command = asum $ map lexeme [ toggleTag
                            , tagShelves
                            , tagFor
                            , tag
                            , move
                            , shelfSel
                            , boxSel
                            , boxRange
                            , swapBoxes
                            , tam
                            , delete
                            , traceCount
                            , traceBoxes
                            , traceShelves
                            , traceOrientations
                            , partitionMode
                            , orientationStrategies
                            , noEmptyBoxes
                            , emptyBoxes
                            , noEmptyShelves
                            , emptyShelves
                            , assert
                            , resizeShelf
                            , resizeShelfFull
                            , resizeBox
                            , splitShelf
                            ] where
   move = do
            exitMode <- (lexeme1 "to^" $> ExitOnTop) <|> (lexeme1 "to>" $> ExitLeft) 
            pmode <- optional $ lexeme1 partitionModeParser
            orules <- (lexeme1 "orules"  >> orientationRules) <|> return []
            shelf <-  shelfSelector
            return $ Move Nothing pmode orules $ singleton (exitMode, shelf)
   tag = do
           lexeme1 "tag"
           tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
           return $ Tag (parseTagOperations tagOps)
   tagFor = withStatement "tag_with" do
       lexeme1 "tag:for"
       selector <- boxSelector
       tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
       return $ TagFor selector (parseTagOperations tagOps)
   toggleTag = do
           lexeme1 "tog"
           tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
           return $ ToggleTags (parseTagOperations tagOps)
   tagShelves = do
           lexeme1 ("tag:shelves" <|> "tag/")
           tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
           return $ TagShelves (parseTagOperations tagOps)
   tam = do
          lexeme1 "tam"
          tagloc <- lexeme1 $ takeWhile1P (Just "loc#tag") (not . isSpace)
          ors <- (lexeme1 "with" >> orientationRules)
                 <|> return []
          return $ TagAndMove tagloc ors
   delete = lexeme1 "delete" >> return Delete
   traceCount = do
       lexeme1 "trace:count"
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ TraceCount desc
       <|> lexeme1 "t:c" $> (TraceCount "T:C")
   traceBoxes = do
       trace <- do
                   lexeme1 "trace:boxes"
                   desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
                   return $ TraceBoxes desc
                <|> lexeme1 "t:b" *> return (TraceBoxes "T:B")
       propM <- optional (lexeme1 "with" >> lexeme1 (takeWhile1P (Just "property") (not . isSpace)))
       return $ trace propM
   traceShelves = do
       lexeme1 "trace:shelves"
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ TraceShelves desc
       <|> lexeme1 "t:s" $> (TraceShelves "T:S")
   traceOrientations = do
       lexeme1 "trace:orules"
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ TraceOrientations desc
       <|> lexeme1 "t:o" $> (TraceOrientations "T:O")
   assert = do
       b <- (lexeme "assert:noboxes"  $> True) <|> (lexeme "assert:boxes" $> False)  
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ AssertBoxes b desc
       <|> lexeme1 "a:nob" $> AssertBoxes True "A:NOBoxes"
       <|> lexeme1 "a:b" $> AssertBoxes False "A:Boxes"
       <|> do 
       b <- (lexeme "assert:noshelves"  $> True) <|> (lexeme "assert:shelves" $> False)  
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ AssertBoxes b desc
       <|> lexeme1 "a:nos" $> AssertShelves True "A:NOBoxes"
       <|> lexeme1 "a:s" $> AssertShelves False "A:Boxes"
   partitionMode = do
       lexeme1  "place"
       pmode <- lexeme1 partitionModeParser
       return $ SetPartitionMode pmode
   orientationStrategies = do
      cons <- (AddOrientationStrategies <$ lexeme1 "orules+")
         <|> (SetOrientationStrategies <$ lexeme1 "orules")
      os <- orientationRules
      cselectorm <- optional $ (lexeme "for" >> cselector parseShelfSelector) <?> "orules:selector"
      selectorm <- case cselectorm of
                        Nothing -> return Nothing
                        Just (CSelector sel) -> return $ Just sel
                        Just sel -> fail $ show sel <> " must be a contextless selector"
      return $ cons selectorm os
   noEmptyBoxes = do
      lexeme1 "empty-boxes:no"
      return $ SetNoEmptyBoxes True
   emptyBoxes = do
      lexeme1 "empty-boxes:yes"
      return $ SetNoEmptyBoxes False
   noEmptyShelves = do
      lexeme1 "empty-shelves:no"
      return $ SetNoEmptyShelves True
   emptyShelves = do
      lexeme1 "empty-shelves:yes"
      return $ SetNoEmptyShelves False
   resizeShelfFull = withStatement  "shelf:full" do
     lexeme1 "shelf:full"
     selector <- shelfSelector
     return $ ResizeShelfFull selector
   resizeShelf = withStatement  "shelf:resize" do
     lexeme1 "shelf:resize"
     selector <- shelfSelector
     l <- lexeme exprParser
     w <- lexeme exprParser
     h <- lexeme exprParser
     return $ ResizeShelf selector Nothing l w h
   resizeBox = withStatement "box:resize" do
     mode <- asum [ lexeme1 "bsize:max"  $> MaxDimension
                  , lexeme1 "bsize:min" $> MinDimension
                  , lexeme1 "bsize:first" $> FirstDimension
                  ]
     selector <- boxSelector
     return $ ResizeBox mode selector

   splitShelf = withStatement "split" do
     lexeme1 "split"
     selector <- shelfSelector
     bselectm <- optional (lexeme "for" *> boxSelector)
     l <- splitExpr
     w <- splitExpr
     h <- splitExpr
     return $ SplitShelf selector bselectm l w h

   boxSel = SelectBoxes <$> boxSelector 
   shelfSel = SelectShelves <$> asum [ lexeme "/"  >> shelfSelector
                                     , lexeme1 "with:boxes" >> return CCrossSelection
                                     , lexeme1 "with" >> cselector parseShelfSelector
                                     ]
   boxRange = do
      boundary <- asum $ [ lexeme1 "from" $> From
                         , lexeme1 "upto" $> Upto
                         , lexeme1 "before" $> Before
                         , lexeme1 "after" $> After
                         ]
      selector <- label "boundary selector" boxSelector
      return $ SelectBoxRanges boundary selector
   swapBoxes = do
      lexeme1 "swap"
      debugPrefix <- optional do 
                              lexeme1 "debug"
                              lexeme $ takeWhile1P (Just "debug") (not . isSpace)  
      stickym <- optional do
                    lexeme1 "sticky"
                    lexeme $ takeWhile1P (Just "tags") (not . isSpace)
      selector <- boxSelector
      let stickies = maybe [] (splitOnNonEscaped "#") stickym
      return $ SwapBoxes selector debugPrefix stickies
                    

withStatement name parser = do
     iLvl <- L.indentLevel
     cons <- lexeme parser
     spaces
     stmt <- (L.indentGuard spaces GT iLvl <?> (name<> show iLvl))
                >> orBlock name
     return $ cons stmt


boxSelector :: MParser (CSelector BoxSelector)
boxSelector =  label "box selector" $ asum
    [  lexeme1 "in:shelves" >> return CCrossSelection
    ,  lexeme1 "in" >> cselector ((\sel -> selectAllBoxes { shelfSelectors = sel} ) . parseSelector)
    , guardLower >> cselector (parseBoxSelectorWithDef False)
    ]

shelfSelector :: MParser (CSelector ShelfSelector)
shelfSelector = label "shelf selector" $ asum
     [ lexeme1 "with:boxes" >> return CCrossSelection
     , lexeme1 "with" >> cselector parseShelfSelector 
     , cselector (ShelfSelector SelectAnything . parseSelector)
     ]
isSelector :: Char -> Bool
isSelector c = not $ isSpace c || c == ')'


cselector :: (Text -> s) -> MParser (CSelector s)
cselector mk = try $  do
        gs <- many go
        selectorm <- case gs of
          [] -> Just <$> sel
          _ -> optional sel
        case gs <> toList selectorm of 
           [] -> fail "some returning []"
           [one] -> return one
           cs -> return $ F.foldr1 CSelectorAnd cs
  where go = asum [swapContext, root, parent] where
        swapContext = do
            (string "-~")
            return SwapContext
        sel = CSelector . mk <$> lexeme1 (takeWhile1P (Just "selector") isSelector)
        parent = (char '~') >> return Parent
        root = (string ".~") >> return Root
         

orientationRules :: MParser [OrientationStrategy]
orientationRules = do
  rule <- lexeme $ takeWhile1P (Just "orientation rules") (not . isSpace)
  case parseOrientationRule [tiltedForward, tiltedFR] rule of
        [] | not (null rule) -> fail "not an rule"
        rules -> return rules

-- | Make sure things don't start with a lower case (to not be mixed
-- with a mispelled command
-- or escape with `
guardLower :: MParser ()
guardLower = label "Escape lower case with `" $ void (char '`') <|> (notFollowedBy lowerChar >> notFollowedBy "[")


splitExpr :: MParser [ Expr Text ]
splitExpr = do
  lexeme $ exprParser `P.sepBy1` lexeme ":"
  
  
   
  
