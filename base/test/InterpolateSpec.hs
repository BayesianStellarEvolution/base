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
  describe "linear interpolation" $ do it "should linearly interpolate"
                                          (linearInterpolate 0.0 0.0 0.0 `shouldBe` 0.0)
