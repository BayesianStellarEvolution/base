module Models.Input where

import Conduit

import Data.Conduit.Lzma
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Data.ByteString (ByteString)
import Data.Ord (comparing)

import MainSequenceModel
import Paths
import Types

loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m [(([ByteString], Double, Double), S.Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel


type EEP = Word

data CAge = CAge LogAge (V.Vector EEP) (V.Vector Mass) [V.Vector Magnitude]
          deriving (Eq, Show)

instance Ord CAge where
  compare = comparing (\(CAge a _ _ _) -> a)

convertModels :: [([ByteString], (Double, Double), S.Set Age)] -> [((FeH, HeliumFraction), S.Set CAge)]
convertModels = map go
  where go (_, (feh, y), isochrone) =
          let feh'  = MkFeH . packLog $ feh
              y'    = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
              iso'  = S.map repackAge isochrone
          in ((feh', y'), iso')
        repackAge (Age age eeps masses magnitudes) =
          let age'    = MkLogAge . packLog $ age
              eeps'   = V.map toEnum eeps
              masses' = repackMass masses
              mags'   = rotateMags magnitudes
          in CAge age' eeps' masses' mags'
        repackMass v = V.map (MkMass . nonNegative') v
        rotateMags v = map (V.map (MkMagnitude . packLog)) v
