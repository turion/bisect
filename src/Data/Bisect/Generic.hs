-- | Generic instances for 'Bisect' which can be derived for all usual datatypes.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Bisect.Generic where

-- generics-sop
import Generics.SOP

-- bisect
import Data.Bisect.Class

gbisect :: forall a . (Generic a, All2 Bisect (Code a), All2 Bounded (Code a), All2 Eq (Code a)) => a -> a -> a
gbisect aL aR = to $ gbisectS (from aL) (from aR)

gbisectS :: (All2 Bisect xss, All2 Bounded xss, All2 Eq xss) => SOP I xss -> SOP I xss -> SOP I xss
gbisectS (SOP (Z aLs)) (SOP (Z aRs)) = SOP $ Z $ gbisectP aLs aRs
gbisectS (SOP (Z _)) (SOP (S (S (Z _)))) = SOP $ S $ Z $ gbisectP minBoundP maxBoundP
gbisectS (SOP (Z _)) (SOP (S aRss@(S _))) = SOP $ S $ unSOP $ gbisectS (SOP (Z minBoundP)) (SOP (predMaxBound aRss))
gbisectS (SOP (S aLss)) (SOP (S aRss)) = SOP $ S $ unSOP $ gbisectS (SOP aLss) (SOP aRss)
-- Unfortunate corner case. Approach the boundary between the constructors from below.
gbisectS (SOP (Z aLs)) (SOP (S (Z aRs))) =
  if and $ hcollapse $ hczipWith (Proxy :: Proxy Eq) (\x y -> K (unI x == unI y)) aLs maxBoundP
  -- We've hit the ceiling for the first constructor
  then SOP $ S $ Z $ gbisectP minBoundP aRs
  -- There is more space in the first constructor
  else SOP $ Z $ gbisectP aLs maxBoundP


predMaxBound :: (All Bounded x, All2 Bounded xs) => NS (NP I) (x ': xs) -> NS (NP I) (x ': xs)
predMaxBound (S (Z _)) = Z minBoundP
predMaxBound (S xs@(S _)) = S $ predMaxBound xs


maxBoundP :: All Bounded xs => NP I xs
maxBoundP = hcpure p $ I maxBound

minBoundP :: All Bounded xs => NP I xs
minBoundP = hcpure p $ I minBound

p :: Proxy Bounded
p = Proxy

-- | Assumes lexicographic ordering
gbisectP :: (All Bounded xs, All Bisect xs) => NP I xs -> NP I xs -> NP I xs
gbisectP Nil Nil = Nil
gbisectP (I aL :* aLs) (I aR :* aRs)
  | aL == aR  = I aL :* gbisectP aLs aRs
  | aL == a   = I aL :* gbisectP aLs maxBoundP
  | a  == aR  = I aR :* gbisectP minBoundP aRs
  | otherwise = I a :* gbisectP aLs aRs
  where
    a = bisect aL aR
