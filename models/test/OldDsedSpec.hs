module MainSequenceSpec where

import Conduit

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Conduit.Lzma
import Data.Conduit.Attoparsec
import Data.Either (isRight)

import Test.Hspec

import MainSequenceModel

loadAndLex :: String -> IO ()
loadAndLex p =
  runConduitRes ( sourceFile p
               .| decompress Nothing
               .| lexModel
               .| parseModel
               .| sinkNull )


main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "Old DSED" $ do
    it "loads successfully" $
      loadAndLex "mainSequence/dsed_old.model.xz"

  describe "New DSED" $ do
    it "loads successfully" $
      loadAndLex "mainSequence/dsed_new.model.xz"

  describe "Yale 2018" $ do
    it "loads successfully" $
      loadAndLex "mainSequence/yale_2018.model"

  describe "PARSEC" $ do
    it "loads successfully" $
      loadAndLex "mainSequence/PARSEC.model"
