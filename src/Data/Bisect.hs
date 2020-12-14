{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bisect where

-- selective
import Control.Selective

-- bisect
import Data.Bisect.Class
import Data.Bisect.Generic

instance (Bounded a, Bounded b, Bisect a, Bisect b) => Bisect (a, b) where
  bisect = gbisect

{- | Find the first element for which a property is true.

@bisectOnInterval p aL aR@ Assumes that the property @p@
is 'False' for some number of elements
on the left side of the interval spanned by @aL@ and @aR@,
and 'True' for all elements on the right side afterwards.
(This number is allowed to be 0, i.e. it can be true for @aL@ already.)
It then repeatedly applies 'bisect' to find the first element for which the property is 'True'.
Returns 'Nothing' if the property is 'False' for all elements.
-}

bisectOnInterval :: (Show a, Bisect a) => (a -> Bool) -> a -> a -> Maybe a
bisectOnInterval p = go
  where
    go aL aR
      | p aL               = Just aL
      | p a                = go aL a
      -- No further elements between aL and aR to test
      | a == aL || a == aR = if p aR then Just aR else Nothing
      | otherwise          = go a aR
      where
        a = bisect aL aR

-- | Like 'bisectOnInterval', but on the whole range of the type.
bisectOn :: (Show a, Bounded a, Bisect a) => (a -> Bool) -> Maybe a
bisectOn p = bisectOnInterval p minBound maxBound

{- | Like the bind operator '(>>=)' of 'Monad',
but resolves the call lazily statically,
and only needs a 'Selective' instance.

FIXME: This still calls a on every branch
-}
bindB :: forall f a b . (Selective f, Bounded a, Bisect a) => f a -> (a -> f b) -> f b
bindB a f = go minBound maxBound
  where
    go :: a -> a -> f b
    go aL aR = branchOrdering (fmap (\a -> (a, compare a aMiddle)) a) (const <$> go aL aMiddle) (f aMiddle) (const <$> go aMiddle aR)
      where
        aMiddle = bisect aL aR

    embedOrdering :: a -> Ordering -> Either a (Either a (Either a b))
    embedOrdering a LT = Left a
    embedOrdering a EQ = Right $ Right $ Left a
    embedOrdering a GT = Right $ Left a

    branchOrdering :: f (a, Ordering) -> f (a -> b) -> f b -> f (a -> b) -> f b
    branchOrdering fao flt feq fgt
      = (uncurry embedOrdering <$> fao)
      <*? fmap (fmap $ Right . Right) flt
      <*? fmap (fmap Right) fgt
      <*? fmap const feq
