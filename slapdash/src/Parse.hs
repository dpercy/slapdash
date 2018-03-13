module Parse where

import Core
import Prelude hiding (lex)

import Data.Char (isSpace, isDigit, isAlphaNum)

data Token = Open
           | Close
           | Equals
           | Integer String
           | Id String
           deriving (Show, Eq)

data ParseError = UnexpectedChar Char
                | UnexpectedEOF
                | UnexpectedToken Token
                | MissingClose
                deriving (Show, Eq)


parse :: String -> Either ParseError Program
parse s = case lex s of
           Left err -> Left err
           Right ts ->
             case parseExpr ts of
              Left err -> Left err
              Right (t:_, _) -> Left (UnexpectedToken t)
              Right ([], e) -> Right (Program [e])


lex :: String -> Either ParseError [Token]
lex "" = Right []
lex (c:cs) | isSpace c = lex cs
lex ('(':cs) = fmap (Open:) (lex cs)
lex (')':cs) = fmap (Close:) (lex cs)
lex ('=':cs) = fmap (Equals:) (lex cs)
lex (    c:cs) | isDigit c = lexInt "" (c:cs)
lex ('-':c:cs) | isDigit c = lexInt "-" (c:cs)
lex (    c:cs) | isIden c = lexIden "" (c:cs)
lex (c:_) = Left (UnexpectedChar c)

lexInt prefix (c:cs) | isDigit c = lexInt (prefix ++ [c]) cs
lexInt prefix cs = fmap (Integer prefix:) (lex cs)

isIden '_' = True
isIden c = isAlphaNum c

lexIden prefix (c:cs) | isIden c = lexIden (prefix ++ [c]) cs
lexIden prefix cs = fmap (Id prefix:) (lex cs)


parseArg :: [Token] -> Either ParseError ([Token], Expr)
parseArg [] = Left UnexpectedEOF
parseArg (Integer s : ts) = Right (ts, Num (read s))
parseArg (Id s : ts) = Right (ts, Var s)
parseArg (Open : ts) = case parseExpr ts of
                        Left err -> Left err
                        Right (Close:ts, e) -> Right (ts, e)
                        Right (_, _) -> Left MissingClose
parseArg (t:ts) = Left (UnexpectedToken t)


parseExpr :: [Token] -> Either ParseError ([Token], Expr)
parseExpr ts = case parseArg ts of
                Left err -> Left err
                Right (ts, f) -> parseCall f ts

parseCall :: Expr -> [Token] -> Either ParseError ([Token], Expr)
parseCall f ts = case parseArg ts of
                  Left err -> Right (ts, f)
                  Right (ts, e) -> parseCall (App f e) ts
