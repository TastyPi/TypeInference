{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses  #-}
{-# LANGUAGE TupleSections, UnicodeSyntax #-}
module Geomlab.Syntax where

import Control.Interpreter

class TypeInferable a b c d where

data Paragraph v =
      Expression (Expr v)
    | Definition (VarDefn v)
    deriving Show

data Expr v =
      Bool Bool
    | Number ℚ
    | Char Char
    | Variable v
    | List [Expr v]
    | Apply (Expr v) [Expr v]
    | If (Expr v) (Expr v) (Expr v)
    | Let (VarDefn v) (Expr v)
    | Lambda [Pattern v] (Expr v)
    deriving Show

data VarDefn v = VarDefn { name :: v, defn :: Defn v }
    deriving Show

data Defn v =
      ValueDef (Expr v)
    | FuncDef (NonEmpty (Clause v))
    deriving Show

data Clause v = Clause [Pattern v] (Expr v) (Maybe (Expr v))
    deriving Show

data Pattern v =
      BlankPattern
    | NumberPattern ℚ
    | StringPattern String
    | NamePattern   v
    | ListPattern   [Pattern v]
    | PlusPattern   (Pattern v) ℚ
    | ConsPattern   (Pattern v) (Pattern v)
    deriving Show

names ∷ Eq v ⇒ Pattern v → [v]
names (PlusPattern p _)  = names p
names (ConsPattern p p') = nub $ names p ++ names p'
names (ListPattern ps)   = foldl union [] $ fmap names ps
names (NamePattern x)    = [x]
names _ = []

instance
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => TypeInferable (Expr x) x v (e m) where
    getType expr =
        case expr of
            Bool _     -> pure (bool, UEnv.empty)
            Number _   -> pure (number, UEnv.empty)
            Char _     -> pure (char, UEnv.empty)
            Variable x -> (id &&& UEnv.singleton x) . UVar <$> lift freeVar
            List es    -> first list <$> foldTypes es

            Apply fe es -> do
                -- Create a fresh variable for the result
                b <- UVar <$> lift freeVar
                -- Return the fresh variable
                -- which has been unified with the actual result type
                first (const b)
                    <$> join
                        -- Unify the actual function type
                        (unifyAndMerge
                            -- The expected type of the function
                            <$> (first (`function` b) <$> getTypes es)
                            -- The actual type of the function
                            <*> getType fe)

            If ce te ee ->
                -- Make sure the condition is bool
                snd <$> (firstM (unify bool) =<< getType ce)
                -- Get the type of the then expression
                >>= (\ env -> secondM (merge env) =<< getType te)
                -- Make sure the else expression matches
                >>= \ (v, env) -> unifyAndMerge (v, env) =<< getType ee

            Let d e ->
                -- Remove the defined variable from the environment
                second (UEnv.delete $ name d)
                    -- Get the type of the definition and add it to the environment
                    <$> (uncurry (UEnv.insert $ name d) <$> getType d
                        -- Analyse the expression
                        >>= \ env -> secondM (merge env) =<< getType e)

            Lambda xs e ->
                -- Remove the argument names from the environment
                second (\ env -> foldr UEnv.delete env $ concatMap names xs)
                    -- Get the type of the function
                    <$> join (kleisliProduct2 (pure2 function) merge <$> getTypes xs <*> getType e)

instance
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => TypeInferable (VarDefn x) x v (e m) where
    getType (VarDefn x d) = second (UEnv.delete x) <$> getType d

instance
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => TypeInferable (Defn x) x v (e m) where
    getType d =
        case d of
            ValueDef e -> getType e
            FuncDef cs -> foldTypes cs

instance
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => TypeInferable (Clause x) x v (e m) where
    getType (Clause args e mwe) =
        -- Create a function type from args to e
        join (kleisliProduct2 (pure2 function) merge <$> getTypes args <*> getType e)
        -- If the when clause exists, unify its type with bool
        >>= secondM
                (maybe
                    return
                    (flip $ \ env -> getType >=> unify bool <⁂> merge env >=> (snd >>> return))
                    mwe)
        -- Remove arguments from the environment
        >>= secondM (return . flip (foldr UEnv.delete) (concatMap names args))

instance
    (Ord x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m))
    => TypeInferable (Pattern x) x v (e m) where
    getType pat =
        case pat of
            BlankPattern      -> (, UEnv.empty) . UVar <$> lift freeVar
            NumberPattern _   -> pure (number, UEnv.empty)
            StringPattern _   -> pure (list char, UEnv.empty)
            NamePattern x     -> (id &&& UEnv.singleton  x) . UVar <$> lift freeVar
            ListPattern ps    -> first list <$> foldTypes ps
            PlusPattern p _   -> firstM (unify number) =<< getType p
            ConsPattern p0 p1 -> join $ unifyAndMerge <$> (first list <$> getType p0) <*> getType p1

instance
    (Ord x, Show x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m), MonadIO (e m))
    => Evaluatable (Paragraph x) x v (e m) where
    evaluate = evaluatePara

evaluatePara ::
    (Ord x, Show x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m), MonadIO (e m))
    => Context x v -> Paragraph x -> e m (Context x v)
evaluatePara cxt p =
    case p of
        Expression e -> evaluateExpr cxt e
        Definition d -> evaluateDefn cxt d

evaluateExpr ::
    (Ord x, Show x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m), MonadIO (e m))
    => Context x v -> Expr x -> e m (Context x v)
evaluateExpr cxt e =
    cxt <$
        (getType e
        >>= uncurry (simplifyInContext cxt)
        >>= (makeShowable >>> print >>> liftIO))

evaluateDefn ::
    (Ord x, Show x,
    MonadTrans e,
    BindingMonad TypeOp v m,
    Applicative (e m), MonadError (UnificationFailure TypeOp v) (e m), MonadIO (e m))
    => Context x v -> VarDefn x -> e m (Context x v)
evaluateDefn cxt d = do
    xct <- getType d >>= uncurry mkContingentType
    liftIO $ putStrLn $ show (name d) ++ " ∷ " ++ show (makeShowable xct)
    return $ Map.insert (name d) xct cxt