module Interpolate where

interpolateIsochrone ::
  (Ord b1, Ord a) =>
  (a, b1)
  -> [((a, b1), b2)]
  -> (([(a, b1)], [(a, b1)]), ([(a, b1)], [(a, b1)]))
interpolateIsochrone (feh, y) model = error "not defined"
