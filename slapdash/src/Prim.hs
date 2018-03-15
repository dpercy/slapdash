module Prim where

import Core
import Rewrite (Rule)

-- TODO typeclass for "representable as Expr"
-- TODO functions for lifting Haskell functions to Rule


rAdd :: Rule
rAdd _ (App (App (Var "add") (Num x)) (Num y)) = Just (Num (x + y))
rAdd _ _ = Nothing

rMul :: Rule
rMul _ (App (App (Var "mul") (Num x)) (Num y)) = Just (Num (x * y))
rMul _ _ = Nothing

rStringAdd :: Rule
rStringAdd _ (App (App (Var "add") (Str x)) (Str y)) = Just (Str (x ++ y))
rStringAdd _ _ = Nothing
