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
        parseOnly parseFilters "%f U\n" == (Right $ Filters ["U"])

      it "parses a list of filters" $
        parseOnly parseFilters "%f U B V R I J H K\n" == (Right $ Filters ["U", "B", "V", "R", "I", "J", "H", "K"])

    describe "Comments" $ do
      let desired = Right $ Comment "any text here"

      it "captures a comment with no space after #" $
        parseOnly parseComment "#any text here\n" == desired
      it "skips tabs after #" $
        parseOnly parseComment "# any text here\n" == desired
      it "skips space after #" $
        parseOnly parseComment "#\tany text here\n" == desired


    describe "Header" $ do
      it "parses a file header" $
        let result = parseOnly parseHeader "# DSED models\n%f U B V R I J H K u g r i z\n"
        in result == Right [Comment "DSED models", Filters ["U", "B", "V", "R", "I", "J", "H", "K", "u", "g", "r", "i", "z"]]

    describe "Model" $ do
      it "parses filters out of the header" $
        parseOnly parseModel "# DSED models\n%f U B V R I J H K u g r i z\n" == (Right $ MSModel ["U","B","V","R","I","J","H","K","u","g","r","i","z"])
