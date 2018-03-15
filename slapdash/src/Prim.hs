{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Prim where

import Core
import Rewrite (Rule, alts)


class Embed a where
  embed :: a -> Expr
  extract :: Expr -> Maybe a

instance Embed Expr where
  embed = id
  extract = Just

instance Embed Integer where
  embed = Num
  extract (Num i) = Just i
  extract _ = Nothing

instance Embed String where
  embed = Str
  extract (Str s) = Just s
  extract _ = Nothing

instance Embed Bool where
  embed True = (Var "true")
  embed False = (Var "false")
  extract (Var "true") = Just True
  extract (Var "false") = Just False
  extract _ = Nothing

binop :: (Embed a, Embed b, Embed c) => String -> (a -> b -> c) -> Rule
binop name op _ (App (App (Var f) x) y)
  | name == f =
      case (extract x, extract y) of
       (Just x', Just y') -> Just (embed (op x' y'))
       _ -> Nothing
binop _ _ _ _ = Nothing

primRules = alts [
  -- TODO parse infix and rename these rules
  binop @Integer "add" (+),
  binop @Integer "mul" (*),
  binop @Integer "sub" (-),
  binop @Integer "div" div, -- except this one is odd
  binop @Integer "lt" (<),
  binop @Integer "lte" (<=),
  binop @Integer "gt" (>),
  binop @Integer "gte" (>=),

  binop @String "add" (++),
  
  binop @Expr "equal" (==)
  ]

