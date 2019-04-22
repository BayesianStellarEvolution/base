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
    it "returns x1 when f = 0 and delta = 0"
       (linearInterpolate 0.0 0.0 0.0 `shouldBe` 0.0)
    it "returns x2 when f = 1 and delta = 0"
       (linearInterpolate 0.0 0.0 1.0 `shouldBe` 0.0)
    it "returns x1 when f = 0"
       (linearInterpolate 0.0 5.0 0.0 `shouldBe` 0.0)
    it "returns x2 when f = 1"
       (linearInterpolate 0.0 5.0 1.0 `shouldBe` 5.0)
