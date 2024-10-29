module WarehousePlanner.WPL.Parser
( wplParser
)
where

import ClassyPrelude hiding(some, many, try)
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector
import WarehousePlanner.Type
import WarehousePlanner.Base
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
lineComment :: MParser ()
lineComment = L.skipLineComment "--"


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
wplParser ::  MParser [Statement]
wplParser = (some $ L.nonIndented spaces $ statement) <*  spaces <* eof


statement :: MParser Statement
statement = asum
   [ indentedBlock <?> "statement:case block"
   , thenMulti <?> "statement:then multi"
   ]  <?> "statement:one"


caseBlock :: MParser Statement
caseBlock = do
  blockOf "case" caseLine Cases

thenBlock :: MParser Statement
thenBlock = do
  blockOf "thens" line mkThen 
  where line = do
          lexeme "&" <?> "& line"
          thenMulti
        mkThen = F.foldr1 Then 
        --       ^^^^^^^^
        --       [a, b, c] -> a Then (b Then c)
indentedBlock = caseBlock <|> thenBlock

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
  a <- atom <?> "line:atom"
  -- if the atom consume a new line, then there is nothing else to to parse
  afterLvl <- L.indentLevel
  -- if iLvl >= afterLvl
  if afterLvl == P.mkPos 1
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
foreachS = do
  iLvl <- L.indentLevel
  lexeme "foreach:shelf"
  spaces
  block <-  (L.indentGuard spaces GT iLvl <?> ("foreach:guard" <> show iLvl))
                     >> (orBlock "foreach" <?> "foreach:children")
  return $ ForeachShelf block

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
    lexeme "|" <?> "start caseline"
    c <- thenMulti
    return case c of
       Then a b -> Case a (Just b)
       _ -> Case c Nothing

atom :: MParser Statement
atom = (PassThrought <$> (lexeme ";" *> statement ))
       <|> ("("  *> statement <* ")")
       <|> foreachS
       <|> (notFollowedBy "|" >> Action <$> command )

command = asum $ map lexeme [ toggleTag
                            , tag
                            , move
                            , shelfSel
                            , boxSel
                            , tam
                            , delete
                            , traceCount
                            , traceBoxes
                            , partitionMode
                            , orientationStrategies
                            , noEmptyBoxes
                            , emptyBoxes
                            , noEmptyShelves
                            , emptyShelves
                            ] where
   move = do
            lexeme1 "to"
            pmode <- optional $ lexeme1 partitionModeParser
            orules <- (lexeme1 "orules"  >> orientationRules) <|> return []
            shelf <-  shelfSelector
            return $ Move Nothing pmode orules shelf
   tag = do
           lexeme1 "tag"
           tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
           return $ Tag (parseTagOperations tagOps)
   toggleTag = do
           lexeme1 "tog"
           tagOps <- lexeme1 $ takeWhile1P (Just "tags") (not . isSpace)
           return $ ToggleTags (parseTagOperations tagOps)
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
   traceBoxes = do
       lexeme1 "trace:boxes"
       desc <- lexeme1 $ takeWhile1P (Just "description") (not . isSpace)
       return $ TraceBoxes desc
   partitionMode = do
       lexeme1  "place"
       pmode <- lexeme1 partitionModeParser
       return $ SetPartitionMode pmode
   orientationStrategies = do
      lexeme1 "orules" <?> "orules:keyword"
      os <- orientationRules
      return $ SetOrientationStrategies os
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


   boxSel = SelectBoxes <$> boxSelector 
   shelfSel = SelectShelves <$> asum [ lexeme "/"  >> shelfSelector
                                     , lexeme1 "with" >> cselector parseShelfSelector
                                     ]


boxSelector :: MParser (CSelector BoxSelector)
boxSelector =  label "box selector" $ asum
    [  lexeme1 "in:shelves" >> return CUseContext
    ,  lexeme1 "in" >> cselector ((\sel -> selectAllBoxes { shelfSelectors = sel} ) . parseSelector)
    , guardLower >> cselector (parseBoxSelectorWithDef False)
    ]

shelfSelector :: MParser (CSelector ShelfSelector)
shelfSelector = label "shelf selector" $ asum
     [ lexeme1 "with" >> cselector parseShelfSelector 
     , lexeme1 "with:boxes" >> return CUseContext
     , cselector (ShelfSelector SelectAnything . parseSelector)
     ]
isSelector :: Char -> Bool
isSelector c = not $ isSpace c || c == ')'


cselector :: (Text -> s) -> MParser (CSelector s)
cselector mk = try $ asum [swapContext, root, parent, stmt, sel ] where
     swapContext = do
         (string "-~")
         return SwapContext
     sel = CSelector . mk <$> lexeme1 (takeWhile1P (Just "selector") isSelector)
     parent = (lexeme $ char '~') >> return Parent
     root = (lexeme $ string ".~") >> return Root
     stmt = CStatement <$> (lexeme "(" *> statement <* lexeme ")")
     -- use = lexeme "ctxt"  >> return CUseContext
         

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
guardLower = label "Escape lower case with `" $ void (char '`') <|> notFollowedBy lowerChar
