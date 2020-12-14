-- | Defines the type class 'Bisect'.

module Data.Bisect.Class where

-- | A type where one can efficiently find an element "between" two other elements.
class Ord a => Bisect a where
  {- | @'bisect' aL aR@ is an element between @aL@ and @aR@,
  ideally in the "middle" of the two elements.

  Law expected to hold:

  @
  forall aL aR . aL <= bisect aL aR <= aR
  @

  It is also assumed that the function is called with @aL <= aR@,
  and the behaviour is undefined if this is not the case.
  -}
  bisect :: a -> a -> a

-- | Like 'bisect', but makes sure that it is called with the arguments in correct order.
--   For example, @'bisect' 1 0@ is undefined, but @'safeBisect' 1 0 == 'bisect' 0 1@.
safeBisect :: Bisect a => a -> a -> a
safeBisect a1 a2 = if a1 > a2 then bisect a2 a1 else bisect a1 a2
