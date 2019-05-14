{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Models.InputSpec (main, spec) where

import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Test.Hspec

import MainSequenceModel (Age(..))
import Models.Input
import Models.Sample
import Types


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "Models.Input" $ do
  describe "convertModels" $ do
    it "Converts between formats in the expected manner" $
       convertModels dsed `shouldBe` convertedDsed


packHeliumFraction = MkHeliumFraction . MkPercentage . closedUnitInterval'
packFeH  = MkFeH . packLog
packAge  = MkLogAge . packLog
packMass = MkMass . nonNegative'
packMag  = MkMagnitude . packLog


convertedDsed :: M.Map FeH (M.Map HeliumFraction (S.Set Isochrone))
convertedDsed =
  [ (packFeH (-2.5),
    [ (packHeliumFraction 0.2451
        , [ Isochrone (packAge 8.39794)
                      [2, 3, 4, 5]
                      (V.map packMass [0.278163, 0.318852, 0.335466, 0.351598])
                      [ ("U", V.map packMag [11.7478, 11.3514, 11.2028, 11.0572])
                      , ("B", V.map packMag [11.0484, 10.7092, 10.5813, 10.4578])
                      , ("V", V.map packMag [9.8499,  9.5412,  9.4241,  9.3119])]
          , Isochrone (packAge 8.477121)
                      [2, 3, 4, 5]
                      (V.map packMass [0.212681, 0.290489, 0.320389, 0.335518])
                      [ ("U", V.map packMag [12.5728, 11.6188, 11.3348, 11.2034])
                      , ("B", V.map packMag [11.7446, 10.9382, 10.6947, 10.5822])
                      , ("V", V.map packMag [10.4768, 9.7498,  9.5277,  9.4251])]])])
  , (packFeH (-2.0),
    [(packHeliumFraction 0.2453
       , [ Isochrone (packAge 8.397940)
                     [2, 3, 4, 5]
                     (V.map packMass [0.297801, 0.335484, 0.338823, 0.355097])
                     [ ("U", V.map packMag [12.1589, 11.8031, 11.7674, 11.5974])
                     , ("B", V.map packMag [11.2562, 10.9432, 10.9126, 10.7646])
                     , ("V", V.map packMag [9.9655,  9.6821,  9.6546,  9.5203])]
         , Isochrone (packAge 8.477121)
                     [2, 3, 4, 5]
                     (V.map packMass [0.251276, 0.317207, 0.335075, 0.337718])
                     [ ("U", V.map packMag [12.6621, 11.9778, 11.8076, 11.7862])
                     , ("B", V.map packMag [11.6918, 11.0959, 10.9477, 10.9296])
                     , ("V", V.map packMag [10.3548, 9.8205,  9.6866,  9.6705])]])])]


convertedNewDsed :: Model
convertedNewDsed = undefined
