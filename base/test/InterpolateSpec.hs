{-# LANGUAGE NoMonomorphismRestriction #-}
module InterpolateSpec (main, spec) where

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as V

import Test.Hspec
import Test.QuickCheck hiding (Positive(..))

import Models.Input
import Models.Sample
import Interpolate
import Types


main :: IO ()
main = hspec spec


shouldBeCloseToD :: (Num a, Ord a, Show a) => a -> a -> a -> Expectation
shouldBeCloseToD delta x1 x2 = abs (x2 - x1) `shouldSatisfy` (< delta)


shouldBeCloseTo :: (Num a, Ord a, Fractional a, Show a) => a -> a -> Expectation
shouldBeCloseTo = shouldBeCloseToD (realToFrac 0.0001)


spec :: SpecWith ()
spec = do
  linearInterpolateSpec
  logInterpolateSpec

  describe "isochrone interpolation" $ do
    it "should return the first when the scaling parameter is 0" $
       (interpolateIsochrones (MkClosedUnitInterval 0.0) i1 i2)
         `shouldBe` (let len = V.length . eeps $ i1
                         trunc = V.drop (len - 1)
                     in (Isochrone (trunc . eeps $ i1)
                                   (trunc . mass $ i1)
                                   (M.map trunc . mags $ i1)))
    it "should return the second when the scaling parameter is 1" $
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

  where unpack :: Log10 -> Double
        unpack = unNonNegative . fromLogSpace


linearInterpolateSpec :: SpecWith ()
linearInterpolateSpec = describe "linear interpolation" $ do
    let zero = closedUnitInterval' 0.0
        one  = closedUnitInterval' 1.0
        half = closedUnitInterval' 0.5

    describe "when delta x = 0" $ do
      it "returns x1 when f = 0"
         (linearInterpolate zero 0.0 0.0 `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate one 0.0 0.0 `shouldBe` 0.0)
      it "returns x1 or x2"
         (linearInterpolate half 0.0 0.0 `shouldBe` 0.0)

    describe "when delta x = 5" $ do
      it "returns x1 when f = 0"
         (linearInterpolate zero 0.0 5.0 `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate one 0.0 5.0 `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate half 0.0 5.0 `shouldBe` 2.5)

    describe "when delta x = 4 and x1 is 1" $ do
      it "returns x1 when f = 0"
         (linearInterpolate zero 1.0 5.0 `shouldBe` 1.0)
      it "returns x2 when f = 1"
         (linearInterpolate one 1.0 5.0 `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate half 1.0 5.0 `shouldBe` 3.0)
