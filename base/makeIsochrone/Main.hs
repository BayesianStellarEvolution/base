module Main where

import Conduit
import Data.Conduit.Attoparsec

import MainSequenceModel

main :: IO ()
main = do
  t <- runConduitRes (sourceFile "/home/elliot/projects/base-models/dsed/dsed_new.model" .| sinkParser parseModel)

  print $ last $ sections t
