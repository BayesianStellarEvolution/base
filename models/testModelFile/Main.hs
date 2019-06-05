module Main where

import Conduit

import Control.Monad (void)
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
          void $ loadModels options >> putStrLn "Loaded model successfully"
  where
    opts = info (option (maybeReader (Just . MkModelFile)) (long "modelFile" <> help "Specify model archive") <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
