module InterpolateSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck hiding (Positive(..))

import Models.Input
import Models.Sample (dsed)
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
    it "should function"
       (interpolateIsochrone (0, 0, 0) (convertModels dsed) `shouldBe` [0, 0, 0])


logInterpolateSpec :: SpecWith ()
logInterpolateSpec = parallel $ do
  describe "log interpolation (per paper)" $ do
    describe "hard-coded" $ do
      it "two average stellar ages" $
         unpack (logInterpolate ((toLogSpace 0) :: Log10) (toLogSpace 5) (closedUnitInterval' 0.5))
           `shouldBeCloseTo` 2.5

    it "is a linear interpolation in log space" $ property $
       \x y f ->
         let x_unpacked = unpack x
             y_unpacked = unpack y
         in unpack (logInterpolate x y f)
              `shouldBeCloseTo`
              linearInterpolate x_unpacked y_unpacked f

  where unpack :: Log10 -> Double
        unpack = unNonNegative . fromLogSpace


linearInterpolateSpec :: SpecWith ()
linearInterpolateSpec = describe "linear interpolation" $ do
    let zero = closedUnitInterval' 0.0
        one  = closedUnitInterval' 1.0
        half = closedUnitInterval' 0.5

    describe "when delta x = 0" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 0.0 0.0 zero `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate 0.0 0.0 one `shouldBe` 0.0)
      it "returns x1 or x2"
         (linearInterpolate 0.0 0.0 half `shouldBe` 0.0)

    describe "when delta x = 5" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 0.0 5.0 zero `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate 0.0 5.0 one `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate 0.0 5.0 half `shouldBe` 2.5)

    describe "when delta x = 4 and x1 is 1" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 1.0 5.0 zero `shouldBe` 1.0)
      it "returns x2 when f = 1"
         (linearInterpolate 1.0 5.0 one `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate 1.0 5.0 half `shouldBe` 3.0)
