module Eval (step, eval, Strategy(..)) where

import SyntaxTree
import Substitution

data Strategy = NormalOrder | CallByValue

step :: Strategy -> Expr -> Maybe Expr
step NormalOrder = stepNormal
step CallByValue = stepCBV

stepNormal :: Expr -> Maybe Expr
stepNormal (App (Lam x body) arg) =
  Just (subst x arg body)

stepNormal (App f arg) =
  case stepNormal f of
    Just f' -> Just (App f' arg)
    Nothing -> App f <$> stepNormal arg

stepNormal (Lam x body) =
  Lam x <$> stepNormal body

stepNormal _ = Nothing

isValue :: Expr -> Bool
isValue (Lam _ _) = True
isValue _         = False

stepCBV :: Expr -> Maybe Expr
stepCBV (App f arg)
  | not (isValue f) = App <$> stepCBV f <*> pure arg
  | not (isValue arg) = App f <$> stepCBV arg
  | Lam x body <- f = Just (subst x arg body)
stepCBV (Lam x body) = Lam x <$> stepCBV body
stepCBV _ = Nothing

eval :: Strategy -> Int -> Expr -> Expr
eval strat limit = go limit
  where
    go 0 e = e
    go n e =
      case step strat e of
        Nothing -> e
        Just e' -> go (n - 1) e'