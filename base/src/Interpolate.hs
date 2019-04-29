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


logInterpolate :: Double -> Double -> ClosedUnitInterval -> Double
logInterpolate x1 x2 f' = let f = coerce f' in (x2 ** f) * (x1 ** (1 - f))
{-
Ref:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/log_proof.txt
-}


logInterpolate_alt :: Double -> Double -> ClosedUnitInterval -> Double
logInterpolate_alt x1 x2 f = exp $ linearInterpolate (log x1) (log x2) f


class Interpolate a where
  interpolate :: a -> a -> ClosedUnitInterval -> a

instance Interpolate Double where
  interpolate a b f = linearInterpolate a b f
