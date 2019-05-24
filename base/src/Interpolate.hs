{-# LANGUAGE FlexibleContexts, StandaloneDeriving, GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Interpolate where

import Control.Exception (Exception, throw)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Models.Input (Model)
import Types


data InterpolationException = EmptyModelException
                            | UnmatchedEEPException
     deriving (Show)

instance Exception InterpolationException


interpolateIsochrone :: Cluster -> Model -> [Double]
interpolateIsochrone c model = []

type HeliumFractionMap = M.Map HeliumFraction LogAgeMap
type LogAgeMap = M.Map LogAge Isochrone

interpolateFeH :: Cluster -> Model -> Isochrone
interpolateFeH c m = go $ M.splitLookup (feh c) m
  where go :: (Model, Maybe HeliumFractionMap, Model)
           -> Isochrone
        go (_, (Just v), _) = interpolateHeliumFraction c v
        go (l,        _, r) = case (null l, null r) of
                                ( True,  True) -> throw EmptyModelException
                                ( True, False) -> interp . M.findMin $ r   -- Note [Extrapolation]
                                (False,  True) -> interp . M.findMax $ l
                                (False, False) -> let l' = M.findMax l
                                                      r' = M.findMin r
                                                      li = interp l'
                                                      ri = interp r'
                                                      f  = interpolationFraction (fst l') (fst r') (feh c)
                                                  in interpolateIsochrones f li ri
        interp = interpolateHeliumFraction c . snd

{-
Note [Extrapolation]
~~~~~~~~~~~~~~~~~~~~

Extrapolation is not allowed by this code, in that, if an interpolation target falls between the left or right boundary (null == True conditions for the either list) and a non-null list,
-}


interpolateHeliumFraction :: Cluster -> HeliumFractionMap -> Isochrone
interpolateHeliumFraction c m = go $ M.splitLookup (heliumFraction c) m
  where go :: (HeliumFractionMap, Maybe LogAgeMap, HeliumFractionMap)
           -> Isochrone
        go (_, (Just v), _) = interpolateLogAge c v
        go (l,        _, r) = case (null l, null r) of
                                ( True,  True) -> throw EmptyModelException
                                ( True, False) -> interp . M.findMin $ r   -- Note [Extrapolation]
                                (False,  True) -> interp . M.findMax $ l
                                (False, False) -> let l' = M.findMax l
                                                      r' = M.findMin r
                                                      li = interp l'
                                                      ri = interp r'
                                                      f  = interpolationFraction (fst l') (fst r') (heliumFraction c)
                                                  in interpolateIsochrones f li ri
        interp = interpolateLogAge c . snd


interpolateLogAge :: Cluster -> LogAgeMap -> Isochrone
interpolateLogAge c m = go $ M.splitLookup (logAge c) m
  where go :: (LogAgeMap, Maybe Isochrone, LogAgeMap)
           -> Isochrone
        go (_, (Just v), _) = v
        go (l,        _, r) = case (null l, null r) of
                                ( True,  True) -> throw EmptyModelException
                                ( True, False) -> snd . M.findMin $ r   -- Note [Extrapolation]
                                (False,  True) -> snd . M.findMax $ l
                                (False, False) -> let l' = M.findMax l
                                                      r' = M.findMin r
                                                      f  = interpolationFraction (fst l') (fst r') (logAge c)
                                                  in interpolateIsochrones f (snd l') (snd r')


interpolateIsochrones :: ClosedUnitInterval -> Isochrone -> Isochrone -> Isochrone
interpolateIsochrones f (Isochrone eeps1 masses1 mags1)
                        (Isochrone eeps2 masses2 mags2) =
  let minEep = max (V.minimum eeps1) (V.minimum eeps2)
      toDrop = V.length . V.takeWhile (< minEep) -- number of records to drop to match EEPs
      drop1  = toDrop eeps1
      drop2  = toDrop eeps2
      dropThenZipWith func v1 v2 =
        let v1' = V.drop drop1 v1
            v2' = V.drop drop2 v2
        in V.zipWith func v1' v2'
      ensureEeps = V.and $ dropThenZipWith (==) eeps1 eeps2
      interp = interpolate f
  in if not ensureEeps
        then throw UnmatchedEEPException
        else Isochrone (V.drop drop1 eeps1)
                       (dropThenZipWith interp masses1 masses2)
                       (M.unionWith (dropThenZipWith interp) mags1 mags2)


{-@ assume linearInterpolate :: (Fractional a) => ClosedUnitInterval -> f:a -> s:a -> {v:a | f <= v && v <= s} @-}
linearInterpolate :: Fractional a => ClosedUnitInterval -> a -> a -> a
linearInterpolate f' x1 x2 = let f = realToFrac . unClosedUnitInterval $ f' in f * x2 + (1 - f) * x1

{-
Note [References]
~~~~~~~~~~~~~~~~~
  eq. 3, published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/linear_proof.txt
-}


linearInterpolationFraction :: Double -> Double -> Double -> ClosedUnitInterval
linearInterpolationFraction l h m =
  let a = m - l
      span = h - l
  in closedUnitInterval_unsafe $ a / span

{-
Note [References]
~~~~~~~~~~~~~~~~~
  eq. 2, published_other/interpolation/log_interpol.pdf
-}



logInterpolate :: LogSpace a => ClosedUnitInterval -> a -> a -> a
logInterpolate (MkClosedUnitInterval 0.0) x1  _ = x1
logInterpolate (MkClosedUnitInterval 1.0)  _ x2 = x2
logInterpolate f x1 x2 = toLogSpace $ nonNegative' $ linearInterpolate f (unpack x1) (unpack x2) -- Note [Log Interpolation]
  where unpack = unNonNegative . fromLogSpace

{-
Note [Log interpolation]
~~~~~~~~~~~~~~~~~~~~~~~~

Log interpolation using the ((x2 ** f) * (x1 ** (1 - f))) equation, despite the
proof (Note [References]), is broken. The result of raising a negative
Fractional (e.g., a non-log value greater than 0 but less than 1) to a
non-integer power is Complex, which results in NaN in many cases.

We may want to look back at this eventually for performance purposes, but it's
possible that the (**) is going through exp/log anyway (i.e., no benefit). One potential fix would be to double memory residency by carrying both log- and non-log-space

Note [References]
~~~~~~~~~~~~~~~~~
  eq. 5, published_other/interpolation/log_interpol.pdf
  unpublished/robinson/interpolation/log_proof.txt
-}


logInterpolationFraction :: LogSpace a => a -> a -> a -> ClosedUnitInterval
logInterpolationFraction l' h' m' = let l = unpack l'
                                        h = unpack h'
                                        m = unpack m'
                                    in linearInterpolationFraction l h m
  where unpack = unNonNegative . fromLogSpace


class Interpolate a where
  interpolate :: ClosedUnitInterval -> a -> a -> a
  interpolationFraction :: a -> a -> a -> ClosedUnitInterval

instance Interpolate Double where
  interpolate = linearInterpolate
  interpolationFraction = linearInterpolationFraction

instance Interpolate NaturalLog where
  interpolate = logInterpolate
  interpolationFraction = logInterpolationFraction

instance Interpolate Log10 where
  interpolate = logInterpolate
  interpolationFraction = logInterpolationFraction

instance Interpolate Log2 where
  interpolate = logInterpolate
  interpolationFraction = logInterpolationFraction

deriving instance Interpolate FeH
deriving instance Interpolate LogAge
deriving instance Interpolate Magnitude

deriving instance Interpolate NonNegative
deriving instance Interpolate Mass

deriving instance Interpolate ClosedUnitInterval
deriving instance Interpolate Percentage
deriving instance Interpolate HeliumFraction
