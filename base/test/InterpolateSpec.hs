module InterpolateSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen

import Interpolate
import Types

main :: IO ()
main = hspec spec

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo x1 x2 = abs (x2 - x1) `shouldSatisfy` (< 0.0001)

instance Arbitrary PositiveDouble where
  arbitrary = PositiveDouble . abs <$> chooseAny

spec :: SpecWith ()
spec = do
  linearInterpolateSpec
  logInterpolateSpec

  describe "isochrone interpolation" $ do
    it "should function"
       (interpolateIsochrone (0, 0, 0) [] `shouldBe` [0, 0, 0])


logInterpolateSpec :: SpecWith ()
logInterpolateSpec = parallel $ do
  describe "log interpolation (per paper)" $ do
    it "is a linear interpolation in log space" $ property $
       \(PositiveDouble x) (PositiveDouble y) (Percentage f) ->
          (logInterpolate (exp x) (exp y) f) `shouldBeCloseTo` exp (linearInterpolate x y f)

  describe "log interpolation (alt)" $ do
    it "is equivalent to the mathematical method" $ property $
       \(PositiveDouble x) (PositiveDouble y) (Percentage f) ->
          (logInterpolate_alt (exp x) (exp y) f) `shouldBeCloseTo` logInterpolate (exp x) (exp y) f


linearInterpolateSpec :: SpecWith ()
linearInterpolateSpec = describe "linear interpolation" $ do
    describe "when delta x = 0" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 0.0 0.0 0.0 `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate 0.0 0.0 1.0 `shouldBe` 0.0)
      it "returns x1 or x2"
         (linearInterpolate 0.0 0.0 0.5 `shouldBe` 0.0)

    describe "when delta x = 5" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 0.0 5.0 0.0 `shouldBe` 0.0)
      it "returns x2 when f = 1"
         (linearInterpolate 0.0 5.0 1.0 `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate 0.0 5.0 0.5 `shouldBe` 2.5)

    describe "when delta x = 4 and x1 is 1" $ do
      it "returns x1 when f = 0"
         (linearInterpolate 1.0 5.0 0.0 `shouldBe` 1.0)
      it "returns x2 when f = 1"
         (linearInterpolate 1.0 5.0 1.0 `shouldBe` 5.0)
      it "returns halfway between x1 and x2 when f = 0.5"
         (linearInterpolate 1.0 5.0 0.5 `shouldBe` 3.0)
