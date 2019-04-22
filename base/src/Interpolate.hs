module Interpolate where

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]


{-
Reference:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/linear_proof.txt
-}
linearInterpolate :: Double -> Double -> Double -> Double
linearInterpolate x1 x2 f = f * x2 + (1 - f) * x1

{-
Reference:
  published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/log_proof.txt
-}
logInterpolate :: Double -> Double -> Double -> Double
logInterpolate x1 x2 f = (x2 ** f) * (x1 ** (1 - f))

logInterpolate_alt :: Double -> Double -> Double -> Double
logInterpolate_alt x1 x2 f = exp $ linearInterpolate (log x1) (log x2) f
