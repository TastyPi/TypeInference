module Types where

import Prelude hiding (fail)

import Control.Monad

import qualified Syntax as S

data TypeEnv a = TypeEnv (Maybe a)

instance Show a => Show (TypeEnv a) where
  show (TypeEnv (Just x)) = show x
  show (TypeEnv Nothing) = "error"

instance Monad TypeEnv where
  return x = TypeEnv (Just x)
  (TypeEnv (Just x)) >>= f = f x
  (TypeEnv Nothing) >>= f = TypeEnv Nothing

data Type =
    Bool
  | Number
  | Char
  | Image
  | List Type
  | Func [Type] Type
  | Variable Integer
  | Any
  deriving Show

isEq :: Type -> Bool
isEq Bool = True
isEq Number = True
isEq Char = True
isEq (List t) = isEq t
isEq _ = False

isOrd :: Type -> Bool
isOrd Number = True
isOrd Char = True
isOrd (List t) = isOrd t
isOrd _ = False

typeOfExpr :: S.Expr -> TypeEnv Type
typeOfExpr (S.Bool _) = return Bool
typeOfExpr (S.Number _) = return Number
typeOfExpr (S.Char _) = return Char
typeOfExpr (S.List es) = liftM List $ foldM unify' Any es
--typeOfExpr (S.Variable x) =
typeOfExpr (S.Apply f args) = do
                                argTs <- mapM typeOfExpr args
                                (Func _ t) <- unify' (Func argTs Any) f
                                return t
typeOfExpr (S.If cond e1 e2) = unify' Bool cond >> unify'' e1 e2
--typeOfExpr (S.Let d e) =
--typeOfExpr (Lambda xs e) =
typeOfExpr _ = fail "Expression has no valid type"

unify :: Type -> Type -> TypeEnv Type
unify Bool Bool = return Bool
unify Number Number = return Number
unify Char Char = return Char
unify Image Image = return Image
unify (List t1) (List t2) = liftM List $ unify t1 t2
unify (Func args1 t1) (Func args2 t2) = do
                                          args <- mapM (uncurry unify) (zip args1 args2)
                                          t <- unify t1 t2
                                          return $ Func args t
--unify (Variable x) t =
unify Any t = return t
unify t Any = return t
unify t1 t2 = fail ("Expected type " ++ show t1 ++ " but actual type was " ++ show t2)

unify' :: Type -> S.Expr -> TypeEnv Type
unify' t e = typeOfExpr e >>= unify t

unify'' :: S.Expr -> S.Expr -> TypeEnv Type
unify'' e1 e2 = typeOfExpr e1 >>= flip unify' e2
