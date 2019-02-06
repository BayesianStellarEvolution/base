module OldDsedSpec where

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
--               .| decompress Nothing
               .| lexModel
               .| parseModel
               .| sinkNull )


main :: IO ()
main = hspec spec

spec = parallel $ do
  xdescribe "Old DSED" $ do
    it "loads successfully" $
      loadAndLex "dsed_old.model.xz"

  xdescribe "New DSED" $ do
    it "loads successfully" $
      loadAndLex "dsed_new.model.xz"

  describe "Yale 2018" $ do
    it "loads successfully" $
      loadAndLex "yale_2018.model"

  describe "PARSEC" $ do
    it "loads successfully" $
      loadAndLex "PARSEC.model"
