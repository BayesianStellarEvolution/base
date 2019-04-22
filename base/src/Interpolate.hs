module Interpolate where

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]

linearInterpolate :: Double -> Double -> Double -> Double
linearInterpolate x1 x2 f = f * x2 + (1 - f) * x1

logInterpolate :: Double -> Double -> Double -> Double
logInterpolate x1 x2 f = (x2 ** f) * (x1 ** (1 - f))

logInterpolate_alt :: Double -> Double -> Double -> Double
logInterpolate_alt x1 x2 f = log $ linearInterpolate (exp x1) (exp x2) f
