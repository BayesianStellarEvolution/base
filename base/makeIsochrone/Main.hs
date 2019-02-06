module Main where

import Conduit

import Control.Monad (when)

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Conduit.Lzma
import Data.Either (isRight)

import MainSequenceModel

main :: IO ()
main = do
  t <- runConduitRes ( sourceFile "/home/elliot/projects/base-models/dsed/dsed_new.model.xz"
                    .| decompress Nothing
                    .| lexModel
                    .| parseModel
                    .| lastC
                     )

  print t
