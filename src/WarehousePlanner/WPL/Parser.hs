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
wplParser = (some $ L.nonIndented spaces $ statement True) <*  spaces <* eof
 

{-
caseBlock :: MParser Statement
caseBlock = do
   L.indentBlock spaces p
   where p = do
           a <- lexeme atom
           e <-  caseLine
           return $ L.IndentSome Nothing  (return . Then a . Cases . (e:|)) caseLine
         caseLine = do
                  lexeme "|"
                  c <- atom
                  st <- statement
                  return $ Case c st
                  -}
     
statement :: Bool -> MParser Statement
statement first = asum
   -- $ drop 1 $
   [ caseBlock first <?> "statement:case block"
   -- , orBlock first
   -- , thenLine first
   , thenMulti first <?> "statement:then multi"
   ] -- <* hspaces <* ( void eol <|> eof)


caseBlock :: Bool -> MParser Statement
caseBlock _first = do
  blockOf caseLine Cases

-- | Statements with same indentation
blockOf :: MParser a -> (NonEmpty a -> b) -> MParser b
blockOf p mk = do
  -- consume possible space to be sure to start at the beginning of 
  -- block to get the indentation correct
  iLvl <- L.indentLevel
  let iguard = try $ L.indentGuard hspaces EQ iLvl <?> ("blockGuard " <> show iLvl)
  cs <- some $ iguard >> p
  case cs of 
     [] -> fail "some returning []"
     c:cs -> return $ mk (c :| cs)

thenLine :: Bool -> MParser Statement
thenLine first =do
  a <- atom first
  let first' = case a of
                Action (SelectBoxes _)  -> False
                _ -> first
  thenm <- optional $ try $ asum [  caseBlock first' <?> "case in line" 
                                 , thenLine first' <?> "next in line"
                                 ]
  case thenm of 
     Nothing -> return a <* (newLine <?> "end of thenLine")
     Just then_ -> return $ a `Then` then_

thenMulti :: Bool -> MParser Statement
thenMulti first = do
  iLvl <- L.indentLevel
  line <- thenLine first <?> "thenMulti:line"
  spaces
  -- get different blocks of decreasing indentation
  childrenm <- many $ (L.indentGuard spaces GT iLvl <?> ("thenMulti:guard " <> show iLvl))
                        >> (orBlock False <?> "thenMulti:children")
  return case childrenm of
         [] -> line
         ors -> F.foldl1 Then (line :| ors)
         --     ^^^^^^^^
         --       [a, b, c] ->  (a Then b) Then c

orBlock first = do
  blockOf ((caseBlock first <?> "caseBlock")
          <|>
          (thenLine first <?> "line block")
          ) mkOrs

mkOrs :: NonEmpty Statement -> Statement     
mkOrs ors = case ors of
   o :| [] -> o
   _ -> Ors ors

  

caseLine :: MParser Case
caseLine = do
    lexeme "|" <?> "start caseline"
    c <- thenLine False
    return case c of 
       Then a b -> Case a (Just b)
       _ -> Case c Nothing
          
atom :: Bool -> MParser Statement
atom first = (PassThrought <$> (lexeme ";" *> statement first))
       <|> ("("  *> statement first <* ")")
       <|> (notFollowedBy "|" >> Action <$> command first)

command first = asum $ map lexeme [ toggleTag
                            , tag
                            , move
                            , shelfSel
                            , boxSel
                            , tam
                            , delete
                            , traceCount
                            , partitionMode
                            , orientationStrategies
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
   partitionMode = do
       lexeme1  "place"
       pmode <- lexeme1 partitionModeParser
       return $ SetPartitionMode pmode
   orientationStrategies = do
      lexeme1 "orules"
      os <- lexeme1 orientationRules
      return $ SetOrientationStrategies os

                    
   boxSel = SelectBoxes <$> boxSelector first
   shelfSel = SelectShelves <$> asum [ lexeme "/"  >> shelfSelector 
                                     , lexeme1 "with" >> cselector parseShelfSelector
                                     ]


boxSelector :: Bool -> MParser (CSelector BoxSelector)
boxSelector first =  label "box selector" $ asum
    [  lexeme1 "in" >> cselector ((\sel -> selectAllBoxes { shelfSelectors = sel} ) . parseSelector)
    , guardLower >> cselector (parseBoxSelectorWithDef $ first == True)
    ]
    
shelfSelector :: MParser (CSelector ShelfSelector)
shelfSelector = label "shelf selector" $ asum 
     [ lexeme1 "with" >> cselector parseShelfSelector
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
     stmt = CStatement <$> (lexeme "(" *> statement False <* lexeme ")")
         
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
