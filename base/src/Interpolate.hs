module Interpolate where

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]

linearInterpolate :: Double -> Double -> Double -> Double
linearInterpolate _ _ _ = 0.0
