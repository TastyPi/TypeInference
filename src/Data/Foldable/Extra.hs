{-# LANGUAGE UnicodeSyntax #-}
module Data.Foldable.Extra (
    -- * Folds
    foldlPairsM, foldrPairsM
) where

import Control.Applicative

import Data.Foldable

import Control.Arrow.Kleisli

-- | Perform a 'foldlM' on a 'Foldable' of pairs.
foldlPairsM ∷ (Foldable f, Applicative m, Monad m) ⇒ (a → b → m a) → (c → d → m c) → (a, c) → f (b, d) → m (a, c)
foldlPairsM f g = foldlM $ kleisliProduct2 f g

-- | Perform a 'foldrM' on a 'Foldable' of pairs.
foldrPairsM ∷ (Foldable f, Monad m) ⇒ (a → a' → m a') → (b → b' → m b') → (a', b') → f (a, b) → m (a', b')
foldrPairsM f g = foldrM $ kleisliProduct2 f g