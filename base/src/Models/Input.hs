module Models.Input (loadModels) where

import Conduit

import Data.Conduit.Lzma
import qualified Data.Set as S

import MainSequenceModel
import Paths
import Types

loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m [((Double, Double), S.Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel


-- type EEP = Positive

-- data CAge = CAge LogAge (V.Vector EEP) (V.Vector Mass) [V.Vector Magnitude] deriving (Eq, Show)

convertModels :: [((Double, Double), S.Set Age)] -> [((FeH, HeliumFraction), S.Set Age)]
convertModels = map go
  where go ((feh, y), isochrone) =
          let feh' = MkFeH . packLog $ feh
              y'   = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
          in ((feh', y'), isochrone)
