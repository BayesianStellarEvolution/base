{-# LANGUAGE FlexibleContexts #-}
module Interpolate where

import Data.Coerce

import Types

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]



linearInterpolate :: (Num a, Fractional a) => a -> a -> ClosedUnitInterval -> a
linearInterpolate x1 x2 f' = let f = realToFrac . unClosedUnitInterval $ f' in f * x2 + (1 - f) * x1
{-
Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/linear_proof.txt
-}


logInterpolate :: LogSpace a => a -> a -> ClosedUnitInterval -> a
logInterpolate x1 x2 f = toLogSpace $ linearInterpolate (fromLogSpace x1) (fromLogSpace x2) f -- Note [Log Interpolation]

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
  interpolate a b f = linearInterpolate a b f
