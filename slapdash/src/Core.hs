module Core where


data Program = Program [Stmt]
             deriving (Show, Eq)

data Stmt = Expr Expr
          | Eqn Equation
          deriving (Show, Eq)

data Expr = Var String
          | App Expr Expr
          | Num Integer
          | Str String
          deriving (Show, Eq)

-- lhs, rhs, condition
type Equation = (Expr, Expr, Maybe Expr)


