module Core where


data Program = Program [Expr]
             deriving (Show, Eq)

data Expr = Var String
          | App Expr Expr
          | Num Integer
          deriving (Show, Eq)

data Rule = Rule Expr Expr
          deriving (Show, Eq)


