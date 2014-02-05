{-# LANGUAGE FlexibleContexts #-}
module Control.Unification.Environment where

import Prelude hiding (sequence)

import Control.Applicative
import Control.Monad (join)
import Control.Monad.Error (MonadError)
import Control.Monad.Trans (MonadTrans)

import Control.Unification

import Data.Traversable

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Arrow.Kleisli

newtype UEnv k t v = UEnv { getMap :: Map k (UTerm t v) }

empty :: UEnv k t v
empty = UEnv Map.empty

singleton :: k -> UTerm t v -> UEnv k t v
singleton k t = UEnv $ Map.singleton k t

delete :: Ord k => k -> UEnv k t v -> UEnv k t v
delete k env = UEnv $ Map.delete k (getMap env)

insert :: Ord k => k -> UTerm t v -> UEnv k t v -> UEnv k t v
insert k t env = UEnv $ Map.insert k t (getMap env)

lookup :: Ord k => k -> UEnv k t v -> Maybe (UTerm t v)
lookup k env = Map.lookup k (getMap env)

merge ::
    (Ord k,
    MonadTrans e,
    BindingMonad t v m,
    Applicative (e m), MonadError (UnificationFailure t v) (e m))
    => UEnv k t v -> UEnv k t v -> e m (UEnv k t v)
merge env0 env1 = UEnv <$> sequence (Map.unionWith (\ t0 t1 -> join $ unify <$> t0 <*> t1) (fmap pure $ getMap env0) (fmap pure $ getMap env1))

unifyAndMerge ::
    (Ord k,
    MonadTrans e,
    BindingMonad t v m,
    Applicative (e m), MonadError (UnificationFailure t v) (e m))
    => (UTerm t v, UEnv k t v) -> (UTerm t v, UEnv k t v) -> e m (UTerm t v, UEnv k t v)
unifyAndMerge = kleisliProduct2 unify merge