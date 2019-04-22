module InterpolateSpec (main, spec) where

import Test.Hspec
import Interpolate

main :: IO ()
main = hspec spec

shouldBeCloseTo :: Double -> Double -> Expectation
shouldBeCloseTo x1 x2 = abs (x2 - x1) `shouldSatisfy` (< 0.0001)

spec :: SpecWith ()
spec = do
  linearInterpolateSpec

  describe "isochrone interpolation" $ do
    it "should function"
       (interpolateIsochrone (0, 0, 0) [] `shouldBe` [0, 0, 0])

  describe "log interpolation (per paper)" $ do
    it "returns x1 when f = 0"
       (logInterpolate (log 1) (log 5) 0.0 `shouldBe` (log 1))
    it "returns x2 when f = 1"
       (logInterpolate (log 1) (log 5) 1.0 `shouldBe` (log 5))
    xit "returns x1 or x2"
        (logInterpolate (log 1) (log 5) 0.5 `shouldBe` (log 1))
    it "returns x1 when f = 0"
       (logInterpolate (log 2) (log 3) 0.0 `shouldBe` (log 2))
    it "returns x2 when f = 1"
       (logInterpolate (log 2) (log 3) 1.0 `shouldBe` (log 3))
    xit "returns x1 or x2"
        (logInterpolate (log 2) (log 3) 0.5 `shouldBe` (log 2))

  describe "log interpolation (alt)" $ do
    it "returns x1 when f = 0"
       (logInterpolate_alt (log 1) (log 5) 0.0 `shouldBe` (log 1))
    it "returns x2 when f = 1"
       (logInterpolate_alt (log 1) (log 5) 1.0 `shouldBe` (log 5))
    xit "returns x1 or x2"
        (logInterpolate_alt (log 1) (log 5) 0.5 `shouldBeCloseTo` (log 3))
    it "returns x1 when f = 0"
       (logInterpolate_alt (log 2) (log 3) 0.0 `shouldBe` (log 2))
    it "returns x2 when f = 1"
       (logInterpolate_alt (log 2) (log 3) 1.0 `shouldBe` (log 3))
    xit "returns x1 or x2"
        (logInterpolate_alt (log 2) (log 3) 0.5 `shouldBe` (log 2))


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
