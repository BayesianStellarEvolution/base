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


data Sample = Sample
  { cluster :: Cluster }

sample :: Parser Sample
sample = Sample
      <$> clusterParser

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample c) = print (feh c)
greet _ = return () >>= \_ -> return ()
