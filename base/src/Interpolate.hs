{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Interpolate where

import Types

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]


{-@ assume linearInterpolate :: (Fractional a) => f:a -> s:a -> ClosedUnitInterval -> {v:a | f <= v && v <= s} @-}
linearInterpolate :: Fractional a => a -> a -> ClosedUnitInterval -> a
linearInterpolate x1 x2 f' = let f = realToFrac . unClosedUnitInterval $ f' in f * x2 + (1 - f) * x1
{-
Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/linear_proof.txt
-}


logInterpolate :: LogSpace a => a -> a -> ClosedUnitInterval -> a
logInterpolate x1 x2 f = toLogSpace $ nonNegative' $ linearInterpolate (unpack x1) (unpack x2) f -- Note [Log Interpolation]
  where unpack = unNonNegative . fromLogSpace

{- Note [Log interpolation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Log interpolation using the ((x2 ** f) * (x1 ** (1 - f))), despite the proof, is
broken. The result of raising a negative Fractional (e.g., a non-log value
greater than 0 but less than 1) to a non-integer power is Complex, which results
in NaN in many cases.

We may want to look back at this eventually for performance purposes, but it's
possible that the (**) is going through exp/log anyway (i.e., no benefit).

Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/log_proof.txt
-}

class Interpolate a where
  interpolate :: a -> a -> ClosedUnitInterval -> a

instance Interpolate Double where
  interpolate = linearInterpolate

instance Interpolate NaturalLog where
  interpolate = logInterpolate

instance Interpolate Log10 where
  interpolate = logInterpolate

instance Interpolate Log2 where
  interpolate = logInterpolate

deriving instance Interpolate FeH
deriving instance Interpolate LogAge
deriving instance Interpolate Magnitude
