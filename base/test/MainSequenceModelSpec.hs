module MainSequenceModelSpec (main, spec) where

import Data.Either (isLeft)
import Test.Hspec
import Data.Attoparsec.Text (parseOnly)

import MainSequenceModel


main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "MS Model file format" $ do
    describe "taggedDouble" $ do
      let doParse = parseOnly $ taggedDouble "tag="

      it "pulls a double given a tag" $
        doParse " tag=1.00" `shouldBe` Right 1.00

      it "requires at lease one space in the source string prior to the tag" $
        doParse "tag=1.00" `shouldSatisfy` isLeft

    describe "Filters" $ do
      let doParse = parseOnly parseFilters

      it "parses a single filter" $
        doParse "%f U\n" `shouldBe` (Right $ Filters ["U"])

      it "parses a list of filters" $
        doParse "%f U B V R I J H K\n" `shouldBe` (Right $ Filters ["U", "B", "V", "R", "I", "J", "H", "K"])


    describe "MS Model section header" $ do
      let doParse = parseOnly parseSectionHeader

      it "parses a section header" $
        let result = doParse "%s [Fe/H]=-2.500000    [alpha/Fe]=0.000000    l/Hp=1.938000    Y=0.245100\n"
        in result `shouldBe` (Right $ SectionHeader (-2.5) 0.0 1.938 0.2451)


    describe "Comments" $ do
      let desired = Right $ Comment "any text here"
          doParse = parseOnly parseComment

      it "captures a comment with no space after #" $
        doParse "#any text here\n" `shouldBe` desired
      it "skips space after #" $
        doParse "#\t any text here\n" `shouldBe` desired


    describe "Header" $ do
      let doParse = parseOnly parseFileHeader

      it "parses a file header" $
        let result = doParse "# DSED models\n%f U B V\n"
        in result `shouldBe` Right [Comment "DSED models", Filters ["U", "B", "V"]]

      it "reads multiple adjacent filter lines as separate lines" $
        let result = doParse "%f U B V\n%f R I J\n"
        in result `shouldBe` Right [Filters ["U", "B", "V"], Filters ["R", "I", "J"]]


    describe "Model" $ do
      let doParse = fmap filters . parseOnly parseModel

      it "parses filters out of the header" $
        let result = doParse "# DSED models\n%f U B V R I J H K u g r i z\n"
        in result `shouldBe` Right ["U","B","V","R","I","J","H","K","u","g","r","i","z"]

      it "concatenates multiple lines of filters" $
        let result = doParse "%f U B V R I J H K\n%f u g r i z\n"
        in result `shouldBe` Right ["U","B","V","R","I","J","H","K","u","g","r","i","z"]
