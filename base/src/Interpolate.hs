module Interpolate where

import Data.Coerce

import Types

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]


linearInterpolate :: Double -> Double -> ClosedUnitInterval -> Double
linearInterpolate x1 x2 f' = let f = coerce f' in f * x2 + (1 - f) * x1
{-
Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/linear_proof.txt
-}


logInterpolate_broken :: Double -> Double -> ClosedUnitInterval -> Double
logInterpolate_broken x1 x2 f' = let f = coerce f' in (x2 ** f) * (x1 ** (1 - f)) -- Note [Log Interpolation]
{-
Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/log_proof.txt
-}

{- Note [Log interpolation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Log interpolation, despite the proof, is broken. The result of raising a
negative Fractional to a non-integer power is Complex, which results in NaN in many cases.

We may want to look back at this eventually for performance purposes, but it's
possible that the (**) is going through exp/log anyway (i.e., no benefit).
-}


logInterpolate :: Double -> Double -> ClosedUnitInterval -> Double
logInterpolate x1 x2 f = log $ linearInterpolate (exp x1) (exp x2) f


class Interpolate a where
  interpolate :: a -> a -> ClosedUnitInterval -> a

instance Interpolate Double where
  interpolate a b f = linearInterpolate a b f
