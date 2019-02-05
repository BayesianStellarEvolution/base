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
               .| decompress Nothing
               .| lexModel
               .| sinkNull )


main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "Old DSED" $ do
    it "loads successfully" $
      loadAndLex "dsed_old.model.xz"

  describe "New DSED" $ do
    it "loads successfully" $
      loadAndLex "dsed_new.model.xz"
