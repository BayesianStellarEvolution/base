{-# LANGUAGE NoMonomorphismRestriction #-}
module InterpolateSpec (main, spec) where

import Data.List (sort)

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

import Test.Hspec
import Test.QuickCheck hiding (Positive(..))

import Debug.Trace

import Models.Input
import Models.Sample
import Interpolate
import Types


main :: IO ()
main = hspec spec


shouldBeCloseToD :: (Num a, Ord a, Show a) => a -> a -> a -> Expectation
shouldBeCloseToD delta x1 x2 = abs (x2 - x1) `shouldSatisfy` (< delta)


shouldBeCloseTo :: (Num a, Ord a, Fractional a, Show a) => a -> a -> Expectation
shouldBeCloseTo = shouldBeCloseToD (realToFrac 0.000001)


spec :: SpecWith ()
spec = do
  linearInterpolateSpec
  logInterpolateSpec

  isochroneSpec


  describe "linear interpolation fraction" $ do
    it "is in closed unit interval" $ property $
       \x y z ->
         let sorted = sort [x, y, z]
             l = sorted !! 0
             m = sorted !! 1
             h = sorted !! 2
             result = if l == h
                         then closedUnitInterval_unsafe 0
                         else closedUnitInterval' $ (m - l) / (h - l)
         in linearInterpolationFraction l h m `shouldBe` result

  describe "log interpolation fraction" $ do
    it "is linear interpolation fraction in log space" $ property $
       \x y z ->
         let sorted = sort [x, y, z]
             l = sorted !! 0
             m = sorted !! 1
             h = sorted !! 2
             lu = unpack l
             mu = unpack m
             hu = unpack h
         in logInterpolationFraction l h m `shouldBe` linearInterpolationFraction lu hu mu
  where unpack :: Log10 -> Double
        unpack = unNonNegative . fromLogSpace


logInterpolateSpec :: SpecWith ()
logInterpolateSpec = parallel $ do
  describe "log interpolation (per paper)" $ do
    describe "hard-coded" $ do
      it "two average stellar ages" $
         unpack (logInterpolate (closedUnitInterval' 0.5) ((toLogSpace 0) :: Log10) (toLogSpace 5))
           `shouldBeCloseTo` 2.5

    it "is a linear interpolation in log space" $ property $
       \f x y ->
         let x_unpacked = unpack x
             y_unpacked = unpack y
         in unpack (logInterpolate f x y)
              `shouldBeCloseTo`
              linearInterpolate f x_unpacked y_unpacked

    it "returns x1 when f = 0.0" $ property $
       \x y -> logInterpolate (closedUnitInterval' 0.0) x y `shouldBe` (x :: Log10)

    it "returns x2 when f = 1.0" $ property $
       \x y -> logInterpolate (closedUnitInterval' 1.0) x y `shouldBe` (y :: Log10)

  where unpack :: Log10 -> Double
        unpack = unNonNegative . fromLogSpace


linearInterpolateSpec :: SpecWith ()
linearInterpolateSpec = describe "linear interpolation" $ do
    it "returns x1 when f = 0.0" $ property $
       \x y -> (linearInterpolate (closedUnitInterval' 0.0) x y `shouldBe` (x :: Double))
    it "returns x2 when f = 1.0" $ property $
       \x y -> (linearInterpolate (closedUnitInterval' 1.0) x y `shouldBe` (y :: Double))
    it "returns halfway between x1 and x2 when f = 0.5" $ property $
       \x y -> (linearInterpolate (closedUnitInterval' 0.5) x y `shouldBe` (0.5 * x + 0.5 * (y :: Double)))


isochroneSpec :: SpecWith ()
isochroneSpec = describe "isochrone interpolation" $ do
    it "returns the first when the scaling parameter is 0.0" $
       (interpolateIsochrones (MkClosedUnitInterval 0.0) i1 i2)
         `shouldBe` (let len = V.length . eeps $ i1
                         trunc = V.drop (len - 1)
                     in (Isochrone (trunc . eeps $ i1)
                                   (trunc . mass $ i1)
                                   (M.map trunc . mags $ i1)))
    it "returns the second when the scaling parameter is 1.0" $
       (interpolateIsochrones (MkClosedUnitInterval 1.0) i1 i2)
         `shouldBe` (let trunc = V.take 1
                     in (Isochrone (trunc . eeps $ i2)
                                   (trunc . mass $ i2)
                                   (M.map trunc . mags $ i2)))
                where i1 = snd . M.findMin . snd . M.findMin . snd . M.findMin . convertModels $ newDsed
                      i2 = snd . M.findMax . snd . M.findMax . snd . M.findMin . convertModels $ newDsed
                      eeps (Isochrone v _ _) = v
                      mass (Isochrone _ v _) = v
                      mags (Isochrone _ _ v) = v
