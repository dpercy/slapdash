module Core where



data Expr = Var String
          | App Expr Expr
          | Num Integer
          deriving (Show, Eq)

data Rule = Rule Expr Expr
          deriving (Show, Eq)


