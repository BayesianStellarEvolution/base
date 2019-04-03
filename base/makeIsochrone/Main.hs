module Main where

import Options

import System.Console.CmdArgs.Implicit


clusterOptions :: [a]
clusterOptions = undefined


data InterpolationMethod = Nearest


main :: IO ()
main = do options <- loadOptions (clusterOptions)
          models <- loadModels (model options)
          let interpolated = interpolateIsochrone Nearest model (cluster options)
          withDB "db.base" $ \db -> storePhotometry interpolated


loadModels :: String -> a
loadModels _ = undefined


withDB :: String -> (a -> b) -> b
withDB _ _ = undefined


interpolateIsochrone :: p1 -> p2 -> p3 -> a
interpolateIsochrone _ _ _ = undefined


storePhotometry :: p -> a
storePhotometry _ = undefined
