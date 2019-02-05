{-# LANGUAGE QuasiQuotes #-}
module MainSequenceModelSpec (main, spec) where

import Conduit

import Data.Attoparsec.ByteString (parseOnly)
import Data.Coerce (coerce)
import Data.Either (isLeft, isRight)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Test.Hspec
import Text.RawString.QQ

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
        let result = doParse "%a logAge=8.397940\n"
        in result `shouldBe` (Right $ AgeHeader 8.397940)


    describe "MS Model EEP" $ do
      let doParse = parseOnly parseEEP "    2 0.278163 11.747800 11.048400  9.849900\n"

      it "parses an EEP line" $
        doParse `shouldBe` (Right $ EEP 2 0.278163 [11.7478, 11.0484, 9.8499])


    describe "Comments" $ do
      let desired = Right $ Comment "any text here"
          doParse = parseOnly parseComment

      it "captures a comment with no space after #" $
        doParse "#any text here\n" `shouldBe` desired
      it "skips space after #" $
        doParse "#\t any text here\n" `shouldBe` desired


    describe "Model" $ do
      it "lexes the sections" $
        let result = sequence $ map (fmap snd) $ runConduitPure $ yield dsed .| lexModel .| sinkList
        in (result `shouldSatisfy` isRight) >> ((\(Right r) -> r) result `shouldBe` expected)
            where expected = [ Comment "(abbreviated) DSED models"
                             , Filters ["U", "B", "V"]
                             , SectionHeader (-2.5) 0 1.938 0.2451
                             , AgeHeader 8.39794
                             , Comment "EEP     Mass         U         B         V"
                             , EEP 2 0.278163 [11.7478, 11.0484, 9.8499]
                             , EEP 3 0.318852 [11.3514, 10.7092, 9.5412]
                             , EEP 4 0.335466 [11.2028, 10.5813, 9.4241]
                             , EEP 5 0.351598 [11.0572, 10.4578, 9.3119]
                             , AgeHeader 8.477121
                             , Comment "EEP     Mass         U         B         V"
                             , EEP 2 0.212681 [12.5728, 11.7446, 10.4768]
                             , EEP 3 0.290489 [11.6188, 10.9382,  9.7498]
                             , EEP 4 0.320389 [11.3348, 10.6947,  9.5277]
                             , EEP 5 0.335518 [11.2034, 10.5822,  9.4251]
                             , Comment ""

                             , SectionHeader (-2.0) 0.0 1.938 0.2453
                             , AgeHeader 8.397940
                             , Comment "EEP     Mass         U         B         V"
                             , EEP 2 0.297801 [12.1589, 11.2562, 9.9655]
                             , EEP 3 0.335484 [11.8031, 10.9432, 9.6821]
                             , EEP 4 0.338823 [11.7674, 10.9126, 9.6546]
                             , EEP 5 0.355097 [11.5974, 10.7646, 9.5203]
                             , AgeHeader 8.477121
                             , Comment "EEP     Mass         U         B         V"
                             , EEP 2 0.251276 [12.6621, 11.6918, 10.3548]
                             , EEP 3 0.317207 [11.9778, 11.0959,  9.8205]
                             , EEP 4 0.335075 [11.8076, 10.9477,  9.6866]
                             , EEP 5 0.337718 [11.7862, 10.9296,  9.6705]]


dsed :: ByteString
dsed = B.pack $ [r|# (abbreviated) DSED models
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
