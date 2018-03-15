module Rewrite where

import Core

import Data.Map (Map)
import qualified Data.Map as Map


-- A Rule is a thing that possibly-rewrites an Expr,
-- but also it needs access to an eval function to check the side conditions (if any).
type Rule = (Expr -> Expr) -> Expr -> Maybe Expr


-- evaluation uses a single (!) reduction rule to reduce an expression to a value
eval :: Rule -> Expr -> Expr
eval step (App f x) = let f' = eval step f in
                       let x' = eval step x in
                        let e' = (App f' x') in
                         case step (eval step) e' of
                          Nothing -> e'
                          Just e'' -> eval step e''
eval step e = case step (eval step) e of
               Nothing -> e
               Just e' -> eval step e'


-- TODO rephrase as foldr orderedChoice failRule
interpEquations :: [Equation] -> Rule
interpEquations [] eval e = Nothing
interpEquations (r:rs) eval e = case interpEquation r eval e of
                                 Just e' -> Just e'
                                 Nothing -> interpEquations rs eval e

interpEquation :: Equation -> Rule
interpEquation (pattern, template, condition) eval expr =
  case tryMatch pattern expr of
   Nothing -> Nothing
   Just subst ->
     case evalCondition eval subst condition of
      True -> Just (applySubst subst template)
      False -> Nothing

-- matching a call-pattern, so treat Var as a literal
tryMatch :: Expr -> Expr -> Maybe (Map String Expr)
tryMatch (Var x) (Var y) | x == y = Just Map.empty
tryMatch (Var _) _ = Nothing
tryMatch (App pf px) (App ef ex) =
  case tryMatch pf ef of
   Nothing -> Nothing
   Just s1 ->
     case tryMatchArg px ex of
      Nothing -> Nothing
      Just s2 -> Just (combineSubsts s1 s2)
tryMatch (App _ _) _ = Nothing
tryMatch (Num x) (Num y) | x == y = Just Map.empty
tryMatch (Num _) _ = Nothing

-- matching an arg-pattern, so treat Var as a hole
tryMatchArg :: Expr -> Expr -> Maybe (Map String Expr)
tryMatchArg (Var x) e = Just (Map.singleton x e)
tryMatchArg p e = tryMatch p e

combineSubsts :: (Map String Expr) -> (Map String Expr) -> (Map String Expr)
-- TODO catch key collisions earlier: when you interpRule
combineSubsts = Map.union


evalCondition :: (Expr -> Expr) -> (Map String Expr) -> Maybe Expr -> Bool
evalCondition eval subst Nothing = True
evalCondition eval subst (Just e) = case eval (applySubst subst e) of
                                     Var "true" -> True
                                     Var "false" -> False
                                     _ -> error "TODO handle non-boolean conditions"

applySubst :: (Map String Expr) -> Expr -> Expr
applySubst s (Var x) = case Map.lookup x s of
                        Nothing -> Var x
                        Just e -> e
applySubst s (App f x) = App (applySubst s f) (applySubst s x)
applySubst s e@(Num _) = e
