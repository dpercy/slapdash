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
                deriving (Show, Eq)

lex :: String -> Either ParseError [Token]
lex "" = Right []
lex (c:cs) | isSpace c = lex cs
lex ('(':cs) = fmap (Open:) (lex cs)
lex (')':cs) = fmap (Close:) (lex cs)
lex ('=':cs) = fmap (Close:) (lex cs)
lex (    c:cs) | isDigit c = lexInt "" (c:cs)
lex (    c:cs) | isIden c = lexIden "" (c:cs)
lex (c:_) = Left (UnexpectedChar c)

lexInt prefix (c:cs) | isDigit c = lexInt (prefix ++ [c]) cs
lexInt prefix cs = fmap (Integer prefix:) (lex cs)

isIden '_' = True
isIden c = isAlphaNum c

lexIden prefix (c:cs) | isIden c = lexIden (prefix ++ [c]) cs
lexIden prefix cs = fmap (Id prefix:) (lex cs)
