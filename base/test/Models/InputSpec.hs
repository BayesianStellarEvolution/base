{-# LANGUAGE OverloadedLists, OverloadedStrings #-}
module Models.InputSpec (main, spec) where

import qualified Data.Vector.Unboxed as V

import Test.Hspec

import Models.Input
import Models.Sample
import Types


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = describe "Models.Input" $ do
  describe "convertModels" $ do
    it "Converts single-y RawModels in the expected manner" $
       convertModels dsed `shouldBe` convertedDsed
    it "Converts multi-y RawModels in the expected manner" $
       convertModels newDsed `shouldBe` convertedNewDsed


packHeliumFraction = MkHeliumFraction . MkPercentage . closedUnitInterval'
packFeH  = MkFeH . packLog
packAge  = MkLogAge . packLog
packMasses = V.map (MkMass . nonNegative')
packMags = V.map (MkAbsoluteMagnitude . packLog)

convertedDsed :: Model
convertedDsed =
  [ ( packFeH (-2.5)
    , [( packHeliumFraction 0.2451
       , [ (packAge 8.397940, Isochrone [2, 3, 4, 5]
                                        (packMasses [0.278163, 0.318852, 0.335466, 0.351598])
                                        [ ("U", packMags [11.7478, 11.3514, 11.2028, 11.0572])
                                        , ("B", packMags [11.0484, 10.7092, 10.5813, 10.4578])
                                        , ("V", packMags [9.8499,  9.5412,  9.4241,  9.3119])])
         , (packAge 8.477121, Isochrone [2, 3, 4, 5]
                                        (packMasses [0.212681, 0.290489, 0.320389, 0.335518])
                                        [ ("U", packMags [12.5728, 11.6188, 11.3348, 11.2034])
                                        , ("B", packMags [11.7446, 10.9382, 10.6947, 10.5822])
                                        , ("V", packMags [10.4768, 9.7498,  9.5277,  9.4251])])])])
  , ( packFeH (-2.0)
    , [( packHeliumFraction 0.2453
       , [ (packAge 8.397940, Isochrone [2, 3, 4, 5]
                                        (packMasses [0.297801, 0.335484, 0.338823, 0.355097])
                                        [ ("U", packMags [12.1589, 11.8031, 11.7674, 11.5974])
                                        , ("B", packMags [11.2562, 10.9432, 10.9126, 10.7646])
                                        , ("V", packMags [9.9655,  9.6821,  9.6546,  9.5203])])
         , (packAge 8.477121, Isochrone [2, 3, 4, 5]
                                        (packMasses [0.251276, 0.317207, 0.335075, 0.337718])
                                        [ ("U", packMags [12.6621, 11.9778, 11.8076, 11.7862])
                                        , ("B", packMags [11.6918, 11.0959, 10.9477, 10.9296])
                                        , ("V", packMags [10.3548, 9.8205,  9.6866,  9.6705])])])])]


convertedNewDsed :: Model
convertedNewDsed =
  [ ( packFeH (-1.0)
    , [ ( packHeliumFraction 0.247800
        , [ (packAge 9.000000, Isochrone [2, 3, 4, 5]
                                         (packMasses [0.113315, 0.124680, 0.140813, 0.173692])
                                         [ ("U", packMags [17.03370, 16.62740, 16.12280, 15.25250])
                                         , ("B", packMags [15.03530, 14.70540, 14.29240, 13.58510])
                                         , ("V", packMags [13.23850, 12.93440, 12.55550, 11.92120])])
          , (packAge 9.096910, Isochrone [2, 3, 4, 5]
                                         (packMasses [0.103069, 0.113581, 0.125209, 0.141832])
                                         [ ("U", packMags [17.44610, 17.02400, 16.60920, 16.09250])
                                         , ("B", packMags [15.36540, 15.02750, 14.69060, 14.26770])
                                         , ("V", packMags [13.54260, 13.23130, 12.92070, 12.53310])])])
      , ( packHeliumFraction 0.33
        , [ (packAge 9.000000, Isochrone [5, 6, 7, 8]
                                         (packMasses [0.152464, 0.191656, 0.235299, 0.256241])
                                         [ ("U", packMags [15.419500, 14.543400, 13.869200, 13.588700])
                                         , ("B", packMags [13.755500, 13.026700, 12.446300, 12.202200])
                                         , ("V", packMags  [12.094600, 11.447100, 10.926900, 10.706600])])
          , (packAge 9.096910, Isochrone [5, 6, 7, 8]
                                         (packMasses [0.128684, 0.153000, 0.191846, 0.232007])
                                         [ ("U", packMags [16.111100, 15.404500, 14.537700, 13.908700])
                                         , ("B", packMags [14.322000, 13.743300, 13.021800, 12.480500])
                                         , ("V", packMags [12.603800, 12.083700, 11.442800, 10.957500])])])])
  , ( packFeH (-0.5)
    , [ ( packHeliumFraction 0.25370
        , [ (packAge 9.000000, Isochrone [2, 3, 4, 5]
                                         (packMasses [0.116263, 0.130317, 0.152523, 0.194038])
                                         [ ("U", packMags [16.931000, 16.564000, 16.068000, 15.288200])
                                         , ("B", packMags [15.136300, 14.815400, 14.381800, 13.705100])
                                         , ("V", packMags [13.420600, 13.116000, 12.706400, 12.077400])])
          , (packAge 9.096910, Isochrone [2, 3, 4, 5]
                                         (packMasses [0.104591, 0.116666, 0.131087, 0.153961])
                                         [ ("U", packMags [17.276200, 16.919900, 16.544800, 16.038100])
                                         , ("B", packMags [15.436600, 15.126700, 14.798700, 14.355800])
                                         , ("V", packMags [13.705400, 13.411400, 13.100200, 12.682000])])])])]
