module MainSequenceModelSpec (main, spec) where

import Test.Hspec
import Data.Attoparsec.Text (parseOnly)

import MainSequenceModel


main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "MS Model file format" $ do
    describe "Filters" $ do
      it "parses a single filter" $
        parseOnly parseFilters "%f U\n" == Right ["U"]
