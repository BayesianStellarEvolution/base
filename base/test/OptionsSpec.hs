{-# LANGUAGE QuasiQuotes, OverloadedLists #-}
module OptionsSpec (main, spec) where

import Conduit

import Data.Attoparsec.ByteString (parseOnly)
import Data.Either (isLeft, isRight)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Test.Hspec


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = pure ()
