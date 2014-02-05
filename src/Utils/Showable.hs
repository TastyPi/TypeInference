{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Utils.Showable (makeShowable, ShowString, vars) where

import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.String

newtype ShowString = ShowString String
    deriving (Eq, Ord, Monoid, IsString)

instance Show ShowString where
    show (ShowString s) = s

greekAlphabet :: [String]
greekAlphabet = map (:"") "αβγδεζηθικλμνξοπρστυφχψω"

vars :: [ShowString]
vars = createVars greekAlphabet

createVars :: [String] -> [ShowString]
createVars varNames = map ShowString varNames ++ map (ShowString "'" `mappend`) (createVars varNames)

makeShowable :: (Functor f, Foldable f, Eq a) => f a -> f ShowString
makeShowable = makeShowableWith greekAlphabet

makeShowableWith :: (Functor f, Foldable f, Eq a) => [String] -> f a -> f ShowString
makeShowableWith vs a = fmap (fromJust . flip lookup subs) a
    where subs = zip (nub $ toList a) (createVars vs)