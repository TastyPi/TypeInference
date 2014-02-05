{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Type.Inferable where

import Prelude hiding (mapM)

import Control.Arrow.Kleisli

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (join)
import Control.Monad.Error (MonadError)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Unification
import Control.Unification.Environment

import Data.Foldable (foldlM)
import Data.Traversable (Traversable, mapM)

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

import Data.Type

import Control.Applicative.Extra

class TypeInferable a k v m | a -> k where
    getType :: a -> m (Type v, UEnv k TypeOp v)

getTypes ::
    (Traversable t,
    Ord k,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m),
    TypeInferable a k v (e m))
    => t a -> e m (Seq (Type v), UEnv k TypeOp v)
getTypes as = join $
                  foldlM (kleisliProduct2 (pure2 (|>)) merge) (Seq.empty, empty)
                      <$> mapM getType as

foldTypes ::
    (Ord k, Traversable t,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m),
    TypeInferable a k v (e m))
    => t a -> e m (Type v, UEnv k TypeOp v)
foldTypes as = join $
                   foldlM unifyAndMerge
                       <$> (, empty) <$> UVar <$> lift freeVar
                       <*> mapM getType as