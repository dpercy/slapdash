module Core where


data Program = Program [Stmt]
             deriving (Show, Eq)

data Stmt = Expr Expr
          | Rule Rule
          deriving (Show, Eq)

data Expr = Var String
          | App Expr Expr
            -- TODO replace Num with Literal
            -- TODO add string literals
          | Num Integer
          deriving (Show, Eq)

-- lhs, rhs, condition
type Rule = (Expr, Expr, Maybe Expr)


