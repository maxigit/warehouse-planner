module WarehousePlanner.WPL.Parser
( wplParser
)
where

import ClassyPrelude hiding(some, many, try)
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector
import WarehousePlanner.Type
-- import WarehousePlanner.Type
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))


-- * Whitespaces & Co
lineComment :: MParser ()
lineComment = L.skipLineComment "--"


blockComment :: MParser ()
blockComment = L.skipBlockComment "{-" "-}"

-- | Spaces with new lines
whites :: MParser ()
whites = L.space space1
                 lineComment
                 blockComment 


-- | Spaces without new lines
spaces :: MParser ()
spaces = L.space (void $ some (char ' ' <|> char '\t'))
                 lineComment
                 empty

lexeme :: MParser a -> MParser a
lexeme = L.lexeme spaces

wplParser ::  MParser [Statement]
wplParser = (some $ L.nonIndented whites statement) <*  whites
 

{-
caseBlock :: MParser Statement
caseBlock = do
   L.indentBlock whites p
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
     
statement :: MParser Statement
statement = do
   iLvl <- L.indentLevel
   a <- lexeme atom
   let iguard =  do
                    eof <|> (void $ L.indentGuard whites GT iLvl)
   nextm <- optional $ try do
         iguard
         statementOrCase
   whites
   case nextm of
      Nothing -> return a
      Just (Left c) -> do
           cs <- some $ iguard >> caseLine
           return $ Then a $ Cases (c :| cs)
      Just (Right s) -> do
           ss <- many $ iguard >> statement
           return $ Then a $ case ss of 
                              [] -> s
                              _ -> Ors (s :| ss )


statementOrCase :: MParser (Either Case Statement)
statementOrCase = fmap Left caseLine <|> fmap Right statement

caseLine :: MParser Case
caseLine = do
    lexeme "|"
    c <- statement
    return case c of 
       Then a b -> Case a (Just b)
       _ -> Case c Nothing
          
atom :: MParser Statement
atom = notFollowedBy "|" >> Action <$> command
  

command = asum $ map lexeme [ move, shelfSelector, boxSelector] where
   move = do 
            lexeme "to" 
            shelf <- lexeme $ takeWhile1P (Just "shelf selector") isSelector
            return $ Move Nothing $ Just $ parseShelfSelector $ "/" <> shelf
   boxSelector = SelectBoxes . parseBoxSelector <$> takeWhile1P (Just "box selector") isSelector
   shelfSelector = "/" >> SelectShelves . ShelfSelector SelectAnything . parseSelector <$> takeWhile1P (Just "shelf selector") isSelector
   isSelector c = not $ isSpace c || c == ')'
{-
  spaces
  parseStatement <* eof

parseStatement :: MParser Statement
parseStatement =  do
    statement <- unit
    (opm, nextm) <- do
              op <- optional $ lexeme ( "~" <|> ";" <|> "|")
              nextm' <- optional parseStatement
              return (op, nextm')

    case nextm of
       Nothing -> return statement
       Just next -> return $ case opm of 
                              Just "~" -> Else statement next
                              Just ";" -> Union statement next
                              Just "|" -> Skip statement next
                              _ -> Then statement next
    where unit = lexeme $ asum [ "(" *> parseStatement <* ")"
                               , parseAction
                               ]
    
-- parseElse :: Statement -> MParser Statement
-- parseElse = "~" *> flip Else <$> parseStatement

parseCommand :: MParser Command
parseCommand = asum $ map lexeme [ move, shelfSelector, boxSelector] where
   move = do 
            "move" 
            shelf <- takeWhile1P (Just "shelf selector") isSelector
            return $ Move Nothing $ Just $ parseShelfSelector shelf
   boxSelector = SelectBoxes . parseBoxSelector <$> takeWhile1P (Just "box selector") isSelector
   shelfSelector = "/" >> SelectShelves . ShelfSelector SelectAnything . parseSelector <$> takeWhile1P (Just "shelf selector") isSelector
   isSelector c = not $ isSpace c || c == ')'
   
parseAction :: MParser Statement
parseAction = Action <$> parseCommand
-}
