{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
module Interpolate where

import Control.Exception (Exception, throw)
import qualified Data.Map as M
import qualified Data.Set as S

import Models.Input (Model)
import Types


data EmptyModelException = EmptyModelException
     deriving (Show)

instance Exception EmptyModelException


interpolateIsochrone :: (Double, Double, Double) -> Model -> [Double]
interpolateIsochrone (feh, y, age) model = [feh, y, age]


interpolateFeH :: FeH -> Model -> S.Set Isochrone
interpolateFeH feh m = go $ M.splitLookup feh m
  where go (_, (Just m), _) = interpolateY m
        go (l,        _, r) = case (null l, null r) of
                                ( True,  True) -> throw EmptyModelException
                                ( True, False) -> interp . M.findMax $ r
                                (False,  True) -> interp . M.findMin $ l
                                (False, False) -> let li = interp . M.findMax $ l
                                                      ri = interp . M.findMin $ r
                                                  in undefined li ri
        interp = interpolateY . snd

interpolateY :: M.Map HeliumFraction (S.Set Isochrone) -> S.Set Isochrone
interpolateY _ = undefined

interpolateAges :: S.Set Isochrone -> Isochrone
interpolateAges _ = undefined

interpolateIsochrones :: Isochrone -> Isochrone -> Isochrone
interpolateIsochrones _ _ = undefined

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
