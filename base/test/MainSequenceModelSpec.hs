module MainSequenceModelSpec (main, spec) where

import Test.Hspec

import MainSequenceModel


main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "MS Model file format" $ do
    describe "Filters" $ do
      it "parses a single filter" $
        parseFilters "%f U" == ["U"]
