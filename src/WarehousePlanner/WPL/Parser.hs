module WarehousePlanner.WPL.Parser
( wplParser
)
where

import ClassyPrelude
import WarehousePlanner.WPL.Types
import WarehousePlanner.Selector
-- import WarehousePlanner.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Char


spaces = L.space space1
                do L.skipLineComment "--"
                do L.skipBlockComment "{-" "-}"
lexeme :: MParser a -> MParser a
lexeme = L.lexeme spaces
wplParser ::  MParser Statement
wplParser = do
  spaces
  parseStatement <* eof


parseStatement :: MParser Statement
parseStatement =  do
    statement <- unit
    (opm, nextm) <- do
              op <- optional $ lexeme ( "~" <|> ";" )
              nextm' <- optional parseStatement
              return (op, nextm')

    case nextm of
       Nothing -> return statement
       Just next -> return $ case opm of 
                              Just "~" -> Else statement next
                              Just ";" -> Union statement next
                              _ -> Then statement next
    where unit = lexeme $ asum [ "(" *> parseStatement <* ")"
                               , parseAction
                               ]
    
-- parseElse :: Statement -> MParser Statement
-- parseElse = "~" *> flip Else <$> parseStatement

parseCommand :: MParser Command
parseCommand = asum $ map lexeme [ move, shelfSelector, boxSelector] where
   move = "move" >> return (Move Nothing Nothing)
   boxSelector = SelectBoxes . parseBoxSelector <$> takeWhile1P (Just "box selector") isSelector
   shelfSelector = ":" >> SelectShelves . parseShelfSelector <$> takeWhile1P (Just "shelf selector") isSelector
   isSelector c = not $ isSpace c || c == ')'
   
parseAction :: MParser Statement
parseAction = Action <$> parseCommand
