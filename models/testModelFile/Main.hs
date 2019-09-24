module Main where

import Conduit

import Control.Monad (void, when)
import Data.Either (lefts, rights)
import Data.Set  (Set)
import Data.Text (Text)

import Options.Applicative

import Paths
import MainSequenceModel

type RawModel = [(([Text], Double, Double), Set Age)]

loadModels :: (HasModelPath p) => p -> IO RawModel
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "") .| lexModel .| parseModel

newtype ModelFile = MkModelFile { unModelFile :: String }

instance HasModelPath ModelFile where
  modelPath a _ = unModelFile a

main :: IO ()
main = do options <- execParser opts
          model <- loadModels options
          putStrLn "Parsed model successfully"

          let eepCheck = map checkEeps model
              leftEeps = lefts eepCheck

          when (not $ null leftEeps) (printLeftEeps leftEeps)
  where
    opts = info (option (maybeReader (Just . MkModelFile)) (long "modelFile" <> help "Specify model archive") <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
    printLeftEeps ls = print ls
