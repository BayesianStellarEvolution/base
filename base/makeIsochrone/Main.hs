module Main where

import Options.Applicative
import Data.Semigroup ((<>))


data Cluster = Cluster { feh :: Double, y :: Double }

clusterParser :: Parser Cluster
clusterParser = Cluster
                <$> option auto
                           (long "clusterFeH"
                         <> metavar "VALUE"
                         <> help "Specify cluster FeH")
                <*> option auto (long "clusterY" <> metavar "VALUE" <> help "Specify cluster Y")


data MakeIsochroneOptions = MakeIsochroneOptions
  { cluster :: Cluster }


sample :: Parser MakeIsochroneOptions
sample = MakeIsochroneOptions
      <$> clusterParser

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters"
     <> header "makeIsochrone" )

greet :: MakeIsochroneOptions -> IO ()
greet (MakeIsochroneOptions c) = print (feh c)
greet _ = return () >>= \_ -> return ()
