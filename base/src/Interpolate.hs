module Interpolate where

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]

linearInterpolate :: Double -> Double -> Double -> Double
linearInterpolate x1 x2 f = f * x2 + (1 - f) * x1
