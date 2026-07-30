data Expr
  = Var String
  | Lam String Expr
  | App Expr Expr