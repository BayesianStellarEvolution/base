module Main where

import qualified Data.Map as M

import Options.Applicative
import Data.Semigroup ((<>))

import Models.Input
import Types

import Interpolate

clusterParser :: Parser Cluster
clusterParser = Cluster
                <$> option (maybeReader (Just . MkFeH . packLog . read))
                      (long "cluster-feh"
                       <> metavar "FEH"
                       <> help "Specify cluster FeH")
                <*> option (maybeReader (fmap (MkHeliumFraction . MkPercentage) . closedUnitInterval . read))
                      (long "cluster-y"
                       <> metavar "Y"
                       <> help "Specify cluster Y")
                <*> option (maybeReader (Just . MkLogAge . packLog . read))
                      (long "cluster-age"
                       <> metavar "AGE"
                       <> help "Specify cluster age in log years")

data MakeIsochroneOptions = MakeIsochroneOptions
  { cluster   :: Cluster
  , modelName :: String }


makeIsochroneOptionParser :: Parser MakeIsochroneOptions
makeIsochroneOptionParser = MakeIsochroneOptions <$> clusterParser <*> option auto (long "model" <> metavar "MODEL" <> help "Specify model. One of:Old_Dsed\nNew_Dsed")


main :: IO ()
main = do options <- execParser opts
          models  <- convertModels <$> loadModels OldDsed
          print $ interpolateIsochrone (cluster options) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
