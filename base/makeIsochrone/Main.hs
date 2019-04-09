module Main where

import qualified Data.Set as S

import Options.Applicative
import Data.Semigroup ((<>))

import Models.Input
import Paths

import MainSequenceModel

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
          print $ S.map PrettyAge $ head $ map snd models
          print $ interpolateIsochrone (feh . cluster $ options, y . cluster $ options) models
  where
    opts = info (makeIsochroneOptionParser <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")


interpolateIsochrone (feh, y) model =
  let fsts = dropWhile ((< feh) . fst) $ map fst model
      (low_fehs, rest)  = span ((<= feh) . fst) fsts
      next = head rest
      high_fehs = takeWhile ((<= fst next) . fst) $ rest
  in (low_fehs, high_fehs)
