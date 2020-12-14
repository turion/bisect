{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Data.Bisect.Num where

-- bisect
import Data.Bisect.Class

-- | Every totally ordered integral 'Num' can be bisected.
newtype BisectNum a = BisectNum { unBisectNum :: a }
  deriving (Eq, Ord, Enum, Real, Integral, Num)

instance (Num a, Integral a, Ord a) => Bisect (BisectNum a) where
  bisect aL aR = (aL + aR) `div` 2

deriving via BisectNum Int instance Bisect Int
