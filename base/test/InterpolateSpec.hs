module InterpolateSpec (main, spec) where

import Test.Hspec
import Interpolate

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do describe "interpolation" $ do
            it "should function"
               (interpolateIsochrone (0, 0, 0) [] `shouldBe` [0, 0, 0])
