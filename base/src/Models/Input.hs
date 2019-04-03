module Models.Input where

import Conduit

import Data.Conduit.Lzma
import qualified Data.Set as S

import MainSequenceModel
import Paths


loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m [((Double, Double), S.Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel
