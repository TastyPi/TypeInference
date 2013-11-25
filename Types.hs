{-# LANGUAGE TupleSections, ImpredicativeTypes, FlexibleInstances, UnicodeSyntax #-}

module Types where

import Prelude hiding (fail)

import Control.Monad
import Control.Monad.ST

import Data.Foldable
import qualified Data.Map as Map
import Data.STRef

import qualified Syntax as S

data Type =
    Bool
  | Number
  | Char
  | Image
  | List Type
  | Func [Type] Type
  | Variable TypeVar
  | Any
  deriving Show

isEq ∷ Type → Bool
isEq Bool = True
isEq Number = True
isEq Char = True
isEq (List t) = isEq t
isEq _ = False

isOrd ∷ Type → Bool
isOrd Number = True
isOrd Char = True
isOrd (List t) = isOrd t
isOrd _ = False

type TypeVar =  Integer

type TypeEnv = Map.Map S.Name TypeVar

emptyEnv ∷ TypeEnv
emptyEnv = Map.empty

getTypeVar ∷ S.Name -> TypeEnv -> Maybe TypeVar
getTypeVar = Map.lookup

newtype T a = T (([TypeVar], [(S.Name, TypeVar)]) -> (a, ([TypeVar], [(S.Name, TypeVar)])))

unwrapT (T t) = t

newTypeVar ∷ T TypeVar
newTypeVar = T $ \((v:vs), assumptions) → (v, (vs, assumptions))

assume :: S.Name -> TypeVar -> T ()
assume x v = T $ \(vs, assumptions) -> ((), (vs, (x, v):assumptions))

instance Monad T where
  return x = T $ \env → (x, env)
  (T t) >>= f = T $ \env -> let (x, env') = t env in unwrapT (f x) env'

typeOfExpr :: S.Expr -> TypeEnv -> T (Maybe Type)
typeOfExpr (S.Bool _) env = return $ Just Bool
typeOfExpr (S.Number _) env = return $ Just Number
typeOfExpr (S.Char _) env = return $ Just Char
typeOfExpr (S.List es) env = liftM (fmap List) $ foldM (\x y -> unify' x y env) (Just Any) es
typeOfExpr (S.Variable x) env = case getTypeVar x env of
                                  Just v -> return $ Just (Variable v)
                                  Nothing -> do
                                               v <- newTypeVar
                                               assume x v
                                               return $ Just (Variable v)
typeOfExpr (S.Apply f args) env = do
                                    margTs <- liftM sequence $ mapM (\arg -> typeOfExpr arg env) args
                                    case margTs of
                                      Nothing -> return Nothing
                                      Just argTs -> do
                                                      β <- newTypeVar
                                                      mft <- unify' (Func argTs (Variable β)) f env
                                                      case mft of
                                                        Just (Func _ t) -> return $ Just t
                                                        _ -> return Nothing
typeOfExpr (S.If cond e1 e2) env = do
                                     mb <- unify' Bool cond env
                                     case mb of
                                       Nothing -> return Nothing
                                       Just _ -> unify' e1 e2 env
--typeOfExpr (S.Let d e) env = addDefn d >> typeOfExpr e
--typeOfExpr (Lambda xs e) env = 

unify :: Type -> Type -> T (Maybe Type)
unify Any t = return $ Just t
unify t Any = return $ Just t
unify Bool Bool = return $ Just Bool
unify Number Number = return $ Just Number
unify Char Char = return $ Just Char
unify Image Image = return $ Just Image
unify (List t1) (List t2) = liftM (fmap List) $ unify t1 t2
unify (Func args1 t1) (Func args2 t2) = do
                                          margs <- liftM sequence $ mapM  (uncurry unify) $ zip args1 args2
                                          case margs of
                                            Nothing -> return Nothing
                                            Just args -> do
                                                           mt <- unify t1 t2
                                                           case mt of
                                                             Nothing -> return Nothing
                                                             Just t -> return $ Just $ Func args t

--unify (Variable x) t =
unify t1 t2 = return Nothing

class Typeable t where
  getType :: t -> TypeEnv -> T (Maybe Type)

instance Typeable Type where
  getType t env = return $ Just t

instance Typeable S.Expr where
  getType = typeOfExpr                                    

instance Typeable t => Typeable (Maybe t) where
  getType (Just t) env = getType t env
  getType Nothing env = return Nothing

unify' :: (Typeable t, Typeable t') => t -> t' -> TypeEnv -> T (Maybe Type)
unify' x y env = do
                    mtx <- getType x env
                    case mtx of
                      Nothing -> return Nothing
                      Just tx -> do
                                   mty <- getType y env
                                   case mty of
                                     Nothing -> return Nothing
                                     Just ty -> unify tx ty
