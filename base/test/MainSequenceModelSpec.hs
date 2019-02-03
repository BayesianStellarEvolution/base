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
          doParse = parseOnly parseComment

      it "captures a comment with no space after #" $
        doParse "#any text here\n" == desired
      it "skips tabs after #" $
        doParse "# any text here\n" == desired
      it "skips space after #" $
        doParse "#\tany text here\n" == desired


    describe "Header" $ do
      it "parses a file header" $
        let result = parseOnly parseHeader "# DSED models\n%f U B V R I J H K u g r i z\n"
        in result == Right [Comment "DSED models", Filters ["U", "B", "V", "R", "I", "J", "H", "K", "u", "g", "r", "i", "z"]]

    describe "Model" $ do
      it "parses filters out of the header" $
        let result = filters <$> parseOnly parseModel "# DSED models\n%f U B V R I J H K u g r i z\n"
        in result == Right ["U","B","V","R","I","J","H","K","u","g","r","i","z"]
