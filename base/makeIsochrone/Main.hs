module Main where

import Options
import Output.Sql
import Models.Input
import Models.Interpolate

import Options.Applicative

clusterOptions :: Parser a
clusterOptions = undefined


main :: IO ()
main = do options <- loadOptions clusterOptions
          models <- loadModels (model options)
          let interpolated = interpolateIsochrone Nearest models (cluster options)
          withDB "db.base" $ \db -> storePhotometry interpolated
