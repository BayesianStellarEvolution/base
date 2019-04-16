module Interpolate where

interpolateIsochrone :: (Double, Double, Double) -> p -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]
