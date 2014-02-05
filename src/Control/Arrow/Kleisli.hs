{-# LANGUAGE UnicodeSyntax #-}
module Control.Arrow.Kleisli (
    -- * Convenience functions for 'Kleisli' arrows
    unkleislify, firstM, secondM, kleisliProduct, (<⁂>), kleisliProduct2, kleisliFanout, (<&&&>)
) where

import Control.Arrow
import Control.Arrow.Unicode

unkleislify ∷ (Kleisli m a b → Kleisli m c d) → (a → m b) → c → m d
unkleislify f = Kleisli ⋙ f ⋙ runKleisli

-- | Convenience function for @first@ with a @Kleisli@ arrow.
firstM ∷ Monad m ⇒ (a → m c) → (a, b) → m (c, b)
firstM = unkleislify first

-- | Convenience function for @second@ with a @Kleisli@ arrow.
secondM ∷ Monad m ⇒ (b → m c) → (a, b) → m (a, c)
secondM = unkleislify second

-- | Convenience function for @(***)@ on 'Kleisli' arrows.
kleisliProduct ∷ Monad m ⇒ (a → m b) → (a' → m b') → (a, a') → m (b, b')
kleisliProduct f g = runKleisli $ Kleisli f ⁂ Kleisli g

infixr 3 <⁂>
-- | Infix @kleisliProduct@.
(<⁂>) ∷ Monad m ⇒ (a → m b) → (c → m d) → (a, c) → m (b, d)
(<⁂>) = kleisliProduct

-- | Convenience function for '(***)' on arbitrary 'Arrows' to 'Kleisli' arrows.
kleisliProduct2 ∷ (Arrow arr, Monad m) ⇒ arr a (b → m c) → arr d (e → m f) → arr (a, d) ((b, e) → m (c, f))
kleisliProduct2 f g = ((f ⋙ arr Kleisli) ⁂ (g ⋙ arr Kleisli)) ⋙ arr (uncurry (⁂) ⋙ runKleisli)

kleisliFanout ∷ Monad m ⇒ (a → m b) → (a → m c) → a → m (b, c)
kleisliFanout f g = runKleisli $ Kleisli f &&& Kleisli g

infixr 3 <&&&>
(<&&&>) ∷ Monad m ⇒ (a → m b) → (a → m c) → a → m (b, c)
(<&&&>) = kleisliFanout