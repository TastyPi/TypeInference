{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Interpreter where

import Prelude hiding (foldl, foldr, lookup)

import Control.Arrow

import Control.Applicative hiding (empty)
import Control.Monad.Error

import Control.Unification
import Control.Unification.IntVar
import Control.Unification.Environment

import Data.Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Type
import Data.Type.Contingent

import Data.String

import Text.Parsec

import Control.Arrow.Kleisli


type Context x v = Map x (ContingentType x v)

interpret :: (MonadIO m, Stream s m t, Evaluatable e x v m) => ParsecT s () m e -> Context x v -> s -> m (Context x v)
interpret parser cxt =
    runParserT parser () "(stdin)"
    >=> either (print >>> liftIO >=> const (return cxt)) (evaluate cxt)

class Evaluatable e x v m where
    evaluate :: Context x v -> e -> m (Context x v)

instance Monad m => Evaluatable e x v m => Evaluatable [e] x v m where
    evaluate = foldlM evaluate

newtype Interpreter a = Interpreter { runInterpreter :: IntBindingT TypeOp IO a }
    deriving (Functor, Applicative, Monad)

instance BindingMonad TypeOp IntVar Interpreter where
    lookupVar = Interpreter . lookupVar
    freeVar = Interpreter freeVar
    bindVar v = Interpreter . bindVar v

instance MonadIO Interpreter where
    liftIO = Interpreter . lift

interpreter ::
    (Ord x, IsString x, Show x, Evaluatable e x IntVar Interpreter )
    => ParsecT String () Interpreter e -> String -> Context x IntVar -> Interpreter a
interpreter parser s cxt = do
    line <- liftIO getLine
    if not (null line)
    then interpreter parser (s ++ "\n" ++ line) cxt
    else interpret parser cxt s >>= interpreter parser ""

simplifyInContext ::
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => Context x v -> Type v -> UEnv x TypeOp v -> e m (ContingentType x v)
simplifyInContext cxt t env0 =
    contextToTypeEnv cxt
    >>= merge env0
    >>= (flip (foldr delete) (Map.keys cxt) >>> mkContingentType t)

contextToTypeEnv ::
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => Context x v -> e m (UEnv x TypeOp v)
contextToTypeEnv cxt = foldM f empty (Map.assocs cxt)
    where f env (x, xct) = do
              (xt, env') <- secondM (merge env . UEnv) =<< extractTypeAndEnv xct
              insert x xt env' <$ maybe (pure ()) (void . unify xt) (lookup x env')

