module InterpolateSpec (main, spec) where

import Test.Hspec
import Interpolate

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  describe "isochrone interpolation" $ do
    it "should function"
       (interpolateIsochrone (0, 0, 0) [] `shouldBe` [0, 0, 0])
  describe "linear interpolation" $ do
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
    describe "log interpolation" $ do
      it "returns x1 when f = 0"
         (logInterpolate (log 1) (log 5) 0.0 `shouldBe` (log 1))
      it "returns x2 when f = 1"
         (logInterpolate (log 1) (log 5) 1.0 `shouldBe` (log 5))
      it "returns x1 or x2"
         (logInterpolate (log 1) (log 5) 0.0 `shouldBe` (log 1))
