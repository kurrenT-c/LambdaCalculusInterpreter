module Substitution (subst, freeVars) where

import SyntaxTree
import Data.Set (Set)
import qualified Data.Set as S

freeVars :: Expr -> Set String
freeVars (Var x)     = S.singleton x
freeVars (Lam x e)   = S.delete x (freeVars e)
freeVars (App f a)   = freeVars f `S.union` freeVars a

fresh :: String -> Set String -> String
fresh x used =
  head $ filter (`S.notMember` used) (x : [x ++ show n | n <- [1..]])

subst :: String -> Expr -> Expr -> Expr
subst x n (Var y)
  | x == y    = n
  | otherwise = Var y

subst x n (Lam y body)
  | y == x = Lam y body
  | y `S.member` freeVars n =
      let y' = fresh y (freeVars body `S.union` freeVars n)
      in Lam y' (subst x n (rename y y' body))
  | otherwise = Lam y (subst x n body)

subst x n (App f a) =
  App (subst x n f) (subst x n a)

rename :: String -> String -> Expr -> Expr
rename old new (Var x)
  | x == old  = Var new
  | otherwise = Var x

rename old new (Lam x body)
  | x == old  = Lam new (rename old new body)
  | otherwise = Lam x (rename old new body)

rename old new (App f a) =
  App (rename old new f) (rename old new a)