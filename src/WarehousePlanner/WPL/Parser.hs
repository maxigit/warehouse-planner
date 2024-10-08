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
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Char
import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad(fail)


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
wplParser = (some $ L.nonIndented whites $ statement True) <*  whites <* eof
 

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
     
statement :: Bool -> MParser Statement
statement first = do
   iLvl <- L.indentLevel
   a <- lexeme $ atom first
   let iguard =  do
                    eof <|> (void $ L.indentGuard whites GT iLvl)
   nextm <- optional $ try do
         iguard
         statementOrCase
   whites
   case nextm of
      Nothing -> return a
      Just (Left c) -> do
           cs <- many $ iguard >> caseLine
           return $ Then a $ Cases (c :| cs)
      Just (Right s) -> do
           ss <- many $ iguard >> statement False
           return $ Then a $ case ss of 
                              [] -> s
                              _ -> Ors (s :| ss )


statementOrCase :: MParser (Either Case Statement)
statementOrCase = fmap Left caseLine <|> fmap Right (statement False)

caseLine :: MParser Case
caseLine = do
    lexeme "|"
    c <- statement False
    return case c of 
       Then a b -> Case a (Just b)
       _ -> Case c Nothing
          
atom :: Bool -> MParser Statement
atom first = (PassThrought <$> (lexeme ";" *> statement first))
       <|> (lexeme "("  *> statement first <* lexeme ")")
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
            lexeme "to" 
            pmode <- optional $ lexeme partitionModeParser
            orules <- (lexeme "orules"  >> orientationRules) <|> return []
            shelf <-  shelfSelector
            return $ Move Nothing pmode orules shelf
   tag = do 
           lexeme "tag"
           tagOps <- lexeme $ takeWhile1P (Just "tags") (not . isSpace)
           return $ Tag (parseTagOperations tagOps) 
   toggleTag = do 
           lexeme "tog"
           tagOps <- lexeme $ takeWhile1P (Just "tags") (not . isSpace)
           return $ ToggleTags (parseTagOperations tagOps) 
   tam = do
          lexeme "tam"
          tagloc <- lexeme $ takeWhile1P (Just "loc#tag") (not . isSpace)
          ors <- (lexeme "with" >> orientationRules)
                 <|> return []
          return $ TagAndMove tagloc ors
   delete = lexeme "delete" >> return Delete
   traceCount = do
       lexeme "trace:count"
       desc <- lexeme $ takeWhile1P (Just "description") (not . isSpace)
       return $ TraceCount desc
   partitionMode = do
       lexeme  "place"
       pmode <- lexeme partitionModeParser
       return $ SetPartitionMode pmode
   orientationStrategies = do
      lexeme "orules"
      os <- lexeme orientationRules
      return $ SetOrientationStrategies os

                    
   boxSel = SelectBoxes <$> boxSelector first
   shelfSel = SelectShelves <$> asum [ lexeme "/"  >> shelfSelector 
                                     , lexeme "with" >> cselector parseShelfSelector
                                     ]


boxSelector :: Bool -> MParser (CSelector BoxSelector)
boxSelector first =  label "box selector" $ asum
    [  lexeme "in" >> cselector ((\sel -> selectAllBoxes { shelfSelectors = sel} ) . parseSelector)
    , guardLower >> cselector (parseBoxSelectorWithDef $ first == True)
    ]
    
shelfSelector :: MParser (CSelector ShelfSelector)
shelfSelector = label "shelf selector" $ asum 
     [ lexeme "with" >> cselector parseShelfSelector
     , cselector (ShelfSelector SelectAnything . parseSelector)
     ]
isSelector :: Char -> Bool
isSelector c = not $ isSpace c || c == ')'
   
   
cselector :: (Text -> s) -> MParser (CSelector s)
cselector mk = try $ asum [swapContext, root, parent, stmt, sel ] where
     swapContext = do
         (string "-~")
         return SwapContext
     sel = CSelector . mk <$> lexeme (takeWhile1P (Just "selector") isSelector)
     parent = (lexeme $ char '~') >> return Parent
     root = (lexeme $ string ".~") >> return Root
     stmt = CStatement <$> (lexeme "(" *> statement False <* ")")
         
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
