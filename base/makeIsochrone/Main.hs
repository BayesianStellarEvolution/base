module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Models.Input
import Paths

data Cluster = Cluster { feh :: Double, y :: Double }

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


data MakeIsochroneOptions = MakeIsochroneOptions
  { cluster :: Cluster }


makeIsochroneOptionParser :: Parser MakeIsochroneOptions
makeIsochroneOptionParser = MakeIsochroneOptions <$> clusterParser


main :: IO ()
main = do options <- execParser opts
          models <- loadModels OldDSED
          print $ map fst models
          print $ interpolateIsochrone (feh . cluster $ options, y . cluster $ options) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")


interpolateIsochrone (feh, y) model = map fst model
