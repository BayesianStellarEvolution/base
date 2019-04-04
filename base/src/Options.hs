module Options where

import Cluster
import Paths

import Options.Applicative
import Data.Semigroup ((<>))

loadOptions :: Parser a -> IO a
loadOptions p = execParser opts
  where opts = info (p <**> helper)
                     ( fullDesc
                    <> progDesc "Print a greeting for TARGET"
                    <> header "hello - a test for optparse-applicative" )


model :: a -> MSModel
model _ = undefined


cluster :: a -> Cluster
cluster _ = undefined
