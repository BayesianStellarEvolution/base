module Main where

import Conduit

import Control.Monad (void)
import Data.Conduit.Lzma
import Data.Set  (Set)
import Data.Text (Text)

import Options.Applicative

import Paths
import MainSequenceModel

type RawModel = [(([Text], Double, Double), Set Age)]

loadModels :: (HasModelPath p) => p -> IO RawModel
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "") .| decompress Nothing .| lexModel .| parseModel

newtype ModelArchive = MkModelArchive { unModelArchive :: String }

instance HasModelPath ModelArchive where
  modelPath a _ = unModelArchive a

main :: IO ()
main = do options <- execParser opts
          void $ loadModels options >> putStrLn "Loaded model successfully"
  where
    opts = info (option (maybeReader (Just . MkModelArchive)) (long "modelFile" <> help "Specify model archive") <**> helper)
      ( fullDesc
     <> progDesc "Generate an isochrone from the models based on cluster parameters")
