module SyntaxTree where

data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  deriving (Eq)

instance Show Expr where
  showsPrec _ (Var x) = showString x
  showsPrec p (Lam x body) =
    showParen (p > 0) $
      showString "\\" . showString x . showString ". " . showsPrec 0 body
  showsPrec p (App f a) =
    showParen (p > 1) $
      showsPrec 1 f . showString " " . showsPrec 2 a