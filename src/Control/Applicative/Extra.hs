{-# LANGUAGE UnicodeSyntax #-}
module Control.Applicative.Extra (
    -- * Lift function results
    pure1, pure2
) where

import Prelude.Unicode

import Control.Applicative

-- | Lift the result of a function.
pure1 ∷ Applicative f ⇒ (a → b) → a → f b
pure1 = (∘) pure

-- | Lift the result of a binary function.
pure2 ∷ Applicative f ⇒ (a → b → c) → a → b → f c
pure2 = (∘) pure1