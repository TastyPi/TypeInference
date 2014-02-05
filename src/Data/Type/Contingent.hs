{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}
module Data.Type.Contingent (ContingentType, extractTypeAndEnv, mkContingentType) where

import Prelude hiding (lookup, null)

import Control.Arrow

import Control.Applicative

-- Concrete Monads
import Control.Monad.Error
import Control.Unification
import Control.Unification.Environment (UEnv)
import qualified Control.Unification.Environment as UEnv

import Data.Data

-- Abstract data structures
import Data.Monoid
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

-- Concrete data structures
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Type

data ValueWithMap k v = VWMap { getVal :: v, getMap :: Map k v}
    deriving (Typeable, Eq, Ord, Data, Functor, Foldable, Traversable)

instance (Ord k, Monoid v) => Monoid (ValueWithMap k v) where
    mempty = VWMap { getVal = mempty, getMap = Map.empty }
    mappend a b =
        VWMap {
            getVal = mappend (getVal a) (getVal b),
            getMap = mappend (getMap a) (getMap b)
        }
    mconcat as =
        VWMap {
            getVal = mconcat (map getVal as),
            getMap = mconcat (map getMap as)
        }

newtype ContingentType k v = CType { getVWMap :: ValueWithMap k (Type v) }
    deriving (Typeable, Functor, Foldable, Traversable)

mkContingentType ::
    (MonadTrans e,
    BindingMonad TypeOp v m,
    MonadError (UnificationFailure TypeOp v) (e m), Functor (e m))
    => Type v -> UEnv k TypeOp v -> e m (ContingentType k v)
mkContingentType t env = CType <$> applyBindingsAll (VWMap t $ UEnv.getMap env)

extractTypeAndEnv ::
    (MonadTrans e,
    BindingMonad TypeOp v m,
    Functor (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => ContingentType k v -> e m (Type v, Map k (Type v))
extractTypeAndEnv ct = (getVal &&& getMap) <$> freshenAll (getVWMap ct)

instance (Show k, Show v) => Show (ContingentType k v) where
    show (CType (VWMap t env)) =
        show t ++
            if Map.null env
            then ""
            else "\nwhen " ++ intercalate "\n and " (fmap (\ (x, xt) -> show x ++ " ∷ " ++ show xt) $ Map.assocs env)