module Main where

import Control.Arrow

import qualified Data.Set as S

import Options.Applicative
import Data.Semigroup ((<>))

import Models.Input
import Paths

import MainSequenceModel
import Interpolate

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
          models  <- loadModels NewDSED
          if length models /= 0 then (print $ S.map PrettyAge $ head $ map snd models) else return ()
          print $ interpolateIsochrone (feh &&& y $ cluster options) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
