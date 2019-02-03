module MainSequenceModelSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "MS Model file format" $ do
    describe "Filters" $ do
      it "passes a test" $ True == True
