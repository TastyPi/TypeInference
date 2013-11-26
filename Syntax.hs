module Syntax where

data Paragraph =
    Expression Expr
  | Definition Defn
  deriving Show

data Expr =
    Bool Bool
  | Number Double
  | Char Char
  | List [Expr]
  | Variable Name
  | Apply Expr [Expr]
  | If Expr Expr Expr
  | Let Defn Expr
  | Lambda [Pattern] Expr
  deriving Show

data Defn =
    ValueDef Name Expr
  | FuncDef Name [Clause]
  deriving Show

type Clause = ([Pattern], Expr, Maybe Expr)

getArgs :: Clause -> [Pattern]
getArgs (as, _, _) = as

getExpr :: Clause -> Expr
getExpr (_, e, _) = e

getWhen :: Clause -> Maybe Expr
getWhen (_, _, w) = w

type Name = String

-- TODO: Pattern matching
type Pattern = Name
