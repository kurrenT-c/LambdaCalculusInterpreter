module SyntaxTree where
data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr
  deriving (Show, Eq)