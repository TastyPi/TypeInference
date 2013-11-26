{-# LANGUAGE TupleSections, ImpredicativeTypes, FlexibleInstances, UnicodeSyntax #-}

module Types where

import Prelude hiding (fail)

import Control.Monad
import Control.Monad.ST

import Data.Foldable
import qualified Data.Map as Map
import Data.Maybe
import Data.STRef

import qualified Syntax as S

data Type =
    TypeVar TypeVar
  | TypeOp TypeOp

type TypeVar = Integer

data TypeOp =
    Bool
  | Number
  | Char
  | List Type
  | Func [Type] Type

type TypeEnv = [(S.Name, Type)]

class Typeable t where
  getType :: t -> TypeEnv -> [TypeVar] -> Maybe Type

instance Typeable S.Name where
  getType x env ngs = listToMaybe $ filter ((x ==) . fst) env

instance Typeable S.Expr where
  getType (S.Bool _)     env ngs = return $ TypeOp Bool
  getType (S.Number _)   env ngs = return $ TypeOp Number
  getType (S.Char _)     env ngs = return $ TypeOp Char
  getType (S.List e)     env ngs = liftM (TypeOp . List) $ getType e env ngs
  getType (S.Variable x) env ngs = getType x env ngs
  getType (S.Apply f es) env ngs = 
