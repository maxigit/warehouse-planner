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
import Control.Monad.Combinators.Expr (makeExprParser, Operator(..))

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
  Left err -> error (P.errorBundlePretty err)
  Right expr -> expr
  

parseExprE :: Text -> Either _ParseError (Expr Text)
parseExprE s =  P.parse (spaces *> exprParser <* P.eof) (unpack s) s 

exprParser :: MParser (Expr Text)
exprParser   = makeExprParser parseTerminalExpr table where
  table = [ [ binary "*" MulE
            , binary "/" DivE
            ]

          , [ binary "+" AddE
            , binary "-" SubE
            ]

          , [ binary "&" MinE
            , binary "|" MaxE
            ]
        ]
  binary op f = InfixL (f <$ op)

parseTerminalExpr :: MParser (Expr Text)
parseTerminalExpr  = parseVal <|> parseExtra  <|> parseGroup 
parseVal :: MParser (Expr Text)
parseVal = do
      neg <- P.option False (P.char '-' $> True)
      v <- P.try P.float <|> fmap fromIntegral P.decimal
      return $ ValE $ if neg then -v else v

parseGroup :: MParser (Expr Text)
parseGroup  = do
  _ <- P.char '('
  spaces
  e <- exprParser  
  spaces
  _ <- P.char ')'
  return e

parseExtra :: MParser (Expr Text)
parseExtra = do
  _ <- P.char '{'
  extra <- P.takeWhileP Nothing (/= '}')
  _ <- P.char '}'
  return $ ExtraE extra


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
