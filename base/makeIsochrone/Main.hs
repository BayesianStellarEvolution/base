module Main where

import Options.Applicative
import Data.Semigroup ((<>))


data Cluster = Cluster { feh :: Double, y :: Double }

clusterParser :: Parser Cluster
clusterParser = Cluster
                <$> option auto (long "clusterFeH" <> metavar "FeH" <> help "Specify cluster FeH")
                <*> option auto (long "clusterY" <> metavar "Y" <> help "Specify cluster Y")


data Sample = Sample
  { cluster :: Cluster
  , hello      :: (String, String)
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> clusterParser
      <*> helloParser
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

helloParser :: Parser (String, String)
helloParser = (,)
              <$> strOption
                    (long "hello"
                     <> metavar "TARGET"
                     <> help "Target for the greeting" )
              <*> strOption
                    (long "last"
                     <> value ""
                     <> metavar "LASTNAME"
                     <> help "test")

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample c h False n) = putStrLn $ "Hello, " ++ show (feh c) ++ replicate n '!'
greet _ = return () >>= \_ -> return ()
