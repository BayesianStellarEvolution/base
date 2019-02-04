{-# LANGUAGE QuasiQuotes #-}
module MainSequenceModelSpec (main, spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Either (isLeft, isRight)

import           Data.Text (Text)
import qualified Data.Text as T

import Test.Hspec
import Text.RawString.QQ

import Debug.Trace

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

    describe "MS Model age header" $ do
      let doParse = parseOnly parseAgeHeader

      it "parses an age header" $
        let result = doParse "%a logAge=8.397940"
        in result `shouldBe` (Right $ AgeHeader 8.397940)


    describe "MS Model EEP" $ do
      let doParse c = parseOnly (parseEEP c) "    2 0.278163 11.747800 11.048400  9.849900\n"

      it "parses an EEP line" $
        doParse 3 `shouldBe` (Right $ EEP 2 0.278163 [11.7478, 11.0484, 9.8499])

      it "fails to parse with too many filters" $
        doParse 2 `shouldSatisfy` isLeft

      it "fails to parse with too few filters" $
        doParse 4 `shouldSatisfy` isLeft


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


    xdescribe "Model" $ do
      let doParse = fmap filters . parseOnly parseModel

      it "parses filters out of the header" $
        let result = doParse "# DSED models\n%f U B V R\n"
        in result `shouldBe` Right ["U", "B", "V", "R"]

      it "concatenates multiple lines of filters" $
        let result = doParse "%f U B\n%f V R\n"
        in result `shouldBe` Right ["U", "B", "V", "R"]

    describe "DSED" $ do
      let doParse = parseOnly parseModel dsed

      it "finds the filters" $
        let result = filters <$> doParse
        in result `shouldBe` Right ["U", "B", "V"]

      it "parses the sections" $
        let result = traceShowId $ sections <$> doParse
        in result `shouldSatisfy` isRight
            where expected =
                    Right [ SectionHeader (-2.5) 0 1.938 0.2451
                          , AgeHeader 8.39794 ]

dsed :: Text
dsed = T.pack $ [r|# (abbreviated) DSED models
%f U B V
%s [Fe/H]=-2.500000    [alpha/Fe]=0.000000    l/Hp=1.938000    Y=0.245100
%a logAge=8.397940
# EEP     Mass         U         B         V
    2 0.278163 11.747800 11.048400  9.8499000
    3 0.318852 11.351400 10.709200  9.5412000
    4 0.335466 11.202800 10.581300  9.424100
    5 0.351598 11.057200 10.457800  9.311900
%a logAge=8.477121
# EEP     Mass         U         B         V
    2 0.212681 12.572800 11.744600 10.476800
    3 0.290489 11.618800 10.938200  9.749800
    4 0.320389 11.334800 10.694700  9.527700
    5 0.335518 11.203400 10.582200  9.425100
%s [Fe/H]=-2.000000    [alpha/Fe]=0.000000    l/Hp=1.938000    Y=0.245300
%a logAge=8.397940
# EEP     Mass         U         B         V
    2 0.297801 12.158900 11.256200  9.965500
    3 0.335484 11.803100 10.943200  9.682100
    4 0.338823 11.767400 10.912600  9.654600
    5 0.355097 11.597400 10.764600  9.520300
%a logAge=8.477121
# EEP     Mass         U         B         V
    2 0.251276 12.662100 11.691800 10.354800
    3 0.317207 11.977800 11.095900  9.820500
    4 0.335075 11.807600 10.947700  9.686600
    5 0.337718 11.786200 10.929600  9.670500
|]
