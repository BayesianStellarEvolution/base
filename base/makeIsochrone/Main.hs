module Main where

import qualified Data.Set as S
import Data.Function

import Options.Applicative
import Data.Semigroup ((<>))

import Models.Input
import Paths

import MainSequenceModel
import Interpolate

data Cluster = Cluster { feh :: Double, y :: Double, age :: Double }

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
          models  <- loadModels OldDSED
          if length models /= 0
             then do
               print $ S.map PrettyAge $ head $ map snd models
               print $ map fst models
             else return ()
          print $ interpolateIsochrone (cluster options & feh, cluster options & y, cluster options & age) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
