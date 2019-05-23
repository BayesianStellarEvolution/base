module Main where

import qualified Data.Map as M

import Options.Applicative
import Data.Semigroup ((<>))

import Models.Input
import Types

import Interpolate

clusterParser :: Parser Cluster
clusterParser = Cluster
                <$> option auto
                      (long "cluster-feh"
                       <> metavar "FEH"
                       <> help "Specify cluster FeH")
                <*> option auto
                      (long "cluster-y"
                       <> metavar "Y"
                       <> help "Specify cluster Y")
                <*> option auto
                      (long "cluster-age"
                       <> metavar "AGE"
                       <> help "Specify cluster age in log years")

data MakeIsochroneOptions = MakeIsochroneOptions
  { cluster :: Cluster }


makeIsochroneOptionParser :: Parser MakeIsochroneOptions
makeIsochroneOptionParser = MakeIsochroneOptions <$> clusterParser


main :: IO ()
main = do options <- execParser opts
          models  <- convertModels <$> loadModels OldDsed
          if length models /= 0
             then do
               print . head . M.toList . M.map M.elems $ models
               print . M.map M.keys $ models
             else return ()
          print $ interpolateIsochrone (cluster options) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
