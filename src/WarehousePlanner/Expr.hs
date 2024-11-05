{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module WarehousePlanner.Expr
( Expr(..)
, parseExpr
, parseExprE
, evalExpr
, exprParser
) where
import ClassyPrelude hiding(readFile)
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P hiding (space)
import Data.Void(Void)

type MParser = P.Parsec Void Text

data Expr extra = AddE (Expr extra) (Expr extra)
          | SubE (Expr extra) (Expr extra)
          | MulE (Expr extra) (Expr extra)
          | DivE (Expr extra) (Expr extra)
          | MinE (Expr extra) (Expr extra)
          | MaxE (Expr extra) (Expr extra)
          | ValE Double
          | ExtraE extra --
     deriving (Show, Eq, Functor, Foldable, Traversable)
          
          
-- * Parsing
spaces :: MParser ()
spaces = P.space >> return ()
parseExpr :: Text -> Expr Text
parseExpr s =  case parseExprE s of
  Left err -> error (show err)
  Right expr -> expr
  

parseExprE :: Text -> Either _ParseError (Expr Text)
parseExprE s =  P.parse (exprParser <* P.eof) (unpack s) s 

exprParser :: MParser (Expr Text)
exprParser   = (P.try (parseMMOp ))
                    <|> parseTerminalExpr 

parseTerminalExpr :: MParser (Expr Text)
parseTerminalExpr  = parseVal <|> parseExtra <|> parseGroup 
parseVal :: MParser (Expr Text)
parseVal = do
      v <- P.try P.float <|> fmap fromIntegral P.decimal
      return $ ValE v

parseGroup :: MParser (Expr Text)
parseGroup  = do
  _ <- P.char '('
  spaces
  e <- exprParser  
  spaces
  _ <- P.char ')'
  return e

parseMulOp :: MParser (Expr Text)
parseMulOp  = P.try p <|> parseTerminalExpr  where
  p = do
    e1 <- parseTerminalExpr 
    spaces
    op <- oneOf "*/"
    spaces
    e2 <- parseMulOp
    let c = case op of
            '*' -> MulE
            '/' -> DivE
            _ -> error "should not happen"
    return $ c e1 e2

parseMMOp :: MParser (Expr Text)
parseMMOp  = P.try p <|> parseAddOp  where
  p = do
    e1 <- parseAddOp 
    spaces
    op <- oneOf "|&"
    spaces
    e2 <- parseMMOp 
    let c = case op of
            '&' -> MinE
            '|' -> MaxE
            _ -> error "should not happen"
    return $ c e1 e2

parseAddOp :: MParser (Expr Text)
parseAddOp = P.try p <|> parseMulOp where
  p = do
    e1 <- parseMulOp 
    spaces
    op <- oneOf "+-"
    spaces
    e2 <- parseAddOp
    let c = case op of
            '+' -> AddE
            '-' -> SubE
            _ -> error "should not happen"
    return $ c e1 e2

parseExtra :: MParser (Expr Text)
parseExtra = do
  _ <- P.char '{'
  extra <- P.takeWhileP Nothing (/= '}')
  _ <- P.char '}'
  return $ ExtraE extra


oneOf :: Text -> MParser Char
oneOf t = P.satisfy (`elem` t)

-- parseRef :: MParser (Expr (RefE 
-- parseRef accessor = do
--   _ <- P.char '{'
--   extra <- P.many (P.noneOf ":}") --  (P.alphaNum <|> P.oneOf ".+-%_\\")
--   acc <- P.option accessor $ P.char ':' *> parseAccessor
--   _ <- P.char '}'
--   return $ ExtraE (pack extra) acc
-- * Eval
evalExpr :: Expr Double -> Double
evalExpr (AddE e1 e2) = evalOperator (+) e1 e2
evalExpr (SubE e1 e2) = evalOperator (-) e1 e2
evalExpr (MulE e1 e2) = evalOperator (*) e1 e2
evalExpr (DivE e1 e2) = evalOperator (/) e1 e2
evalExpr (MinE e1 e2) = evalOperator min e1 e2
evalExpr (MaxE e1 e2) = evalOperator max e1 e2
evalExpr (ValE v) = v
evalExpr (ExtraE v) = v

evalOperator op e1 e2 = evalExpr e1 `op` evalExpr e2
