module Models.Input (loadModels) where

import Conduit

import Data.Conduit.Lzma
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Data.Ord (comparing)
import Data.Word

import MainSequenceModel
import Paths
import Types

loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m [((Double, Double), S.Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel


type EEP = Word

data CAge = CAge LogAge (V.Vector EEP) -- (V.Vector Mass) [V.Vector Magnitude] deriving (Eq, Show)
          deriving (Eq)

instance Ord CAge where
  compare = comparing (\(CAge a _) -> a)

convertModels :: [((Double, Double), S.Set Age)] -> [((FeH, HeliumFraction), S.Set CAge)]
convertModels = map go
  where go ((feh, y), isochrone) =
          let feh' = MkFeH . packLog $ feh
              y'   = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
              iso' = S.map repackAge isochrone
          in ((feh', y'), iso')
        repackAge (Age age eeps masses magnitudes) =
          let age'  = MkLogAge . packLog $ age
              eeps' = V.map toEnum eeps
          in CAge age' eeps'
