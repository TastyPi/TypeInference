{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Data.Type (
    -- * Types for types
    Type, TypeOp(TypeOp), Op,
    -- ** Types
    bool, number, char, list, function,
    -- ** Type Operators
    boolOp, numberOp, charOp, listOp, functionOp,
    -- * Unificaton
    unify
) where

-- Used for unifying types
import Control.Unification

import Control.Arrow ((>>>))

-- Algebraic structures
import Data.Monoid

-- General data structures
import Data.Foldable (Foldable, foldMap, toList)
import Data.Traversable (Traversable)

-- Data structures
import Data.Sequence (Seq, (|>), ViewR((:>)))
import qualified Data.Sequence as Seq
import Data.String (IsString, fromString)

-- | The type of types. @UTerm@s are type operators and @UVar@s are type variables.
-- Use @Control.Unification.unify@ to unify them
type Type = UTerm TypeOp

-- | A type operator applied to types
data TypeOp v -- ^ Paramaterised to allow type variables.
    = TypeOp Op (Seq v)
    deriving (Eq, Functor, Foldable, Traversable)

-- | A type operator. @Function@ is only separated for nice formatting.
data Op = Function | Named String
    deriving (Eq)

instance IsString Op where
    fromString = Named

boolOp :: TypeOp v
boolOp = TypeOp "Bool" Seq.empty

numberOp :: TypeOp v
numberOp = TypeOp "Number" Seq.empty

charOp :: TypeOp v
charOp = TypeOp "Char" Seq.empty

listOp :: v -> TypeOp v
listOp = Seq.singleton >>> TypeOp "List"

functionOp :: Seq v -> v -> TypeOp v
functionOp vs v = TypeOp Function (vs |> v)

bool :: Type v
bool = UTerm boolOp

number :: Type v
number = UTerm numberOp

char :: Type v
char = UTerm charOp

list :: Type v -> Type v
list t = UTerm (listOp t)

function :: Seq (Type v) -> Type v -> Type v
function ts t = UTerm (functionOp ts t)

instance Unifiable TypeOp where
    zipMatch (TypeOp op0 ts0) (TypeOp op1 ts1) =
        if op0 == op1 && Seq.length ts0 == Seq.length ts1
        then Just $ TypeOp op0 (fmap Right $ Seq.zip ts0 ts1)
        else Nothing

instance Show v => Show (TypeOp v) where
    showsPrec d (TypeOp op ts) =
        case op of
            Named x -> showParen (d > 10) $ (x ++) . appEndo (foldMap (Endo . (>>> (' ' :)) . showsPrec 11) ts)
            Function ->
                case Seq.viewr ts of
                    args :> res -> showParen (d > 5) $ showsArgs args . (" → " ++) . shows res
                    _ -> error "Invalid function"
        where
            showsArgs args =
                case Seq.viewr args of
                    Seq.EmptyR -> ("()" ++)
                    as :> a -> appEndo (mconcat (map (\arg -> Endo $ showsPrec 6 arg . (" ⨯ " ++)) $ toList as)) . showsPrec 6 a

instance Show Op where
    show Function = "(→)"
    show (Named x) = x