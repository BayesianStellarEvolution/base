{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Models.InputSpec (main, spec) where

import Test.Hspec

import Models.Input
import Models.Sample
import Models.SampleConverted

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "Models.Input" $ do
  describe "convertModels" $ do
    it "Converts single-y RawModels in the expected manner" $
       convertModels dsed `shouldBe` convertedDsed
    it "Converts multi-y RawModels in the expected manner" $
       convertModels newDsed `shouldBe` convertedNewDsed
