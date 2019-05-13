module Models.Input where

import Conduit

import Data.Conduit.Lzma
import Data.ByteString   (ByteString)
import Data.Ord          (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import MainSequenceModel
import Paths
import Types

loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m [(([ByteString], Double, Double), S.Set Age)]
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel


convertModels :: [(([ByteString], Double, Double), S.Set Age)] -> [((FeH, HeliumFraction), S.Set Isochrone)]
convertModels = map go
  where go ((fs, feh, y), isochrone) =
          let feh'  = MkFeH . packLog $ feh
              y'    = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
              iso'  = S.map (repackAge fs) isochrone
          in ((feh', y'), iso')
        repackAge fs (Age age eeps masses magnitudes) =
          let age'    = MkLogAge . packLog $ age
              eeps'   = V.map toEnum eeps
              masses' = repackMass masses
              mags'   = repackMags fs magnitudes
          in Isochrone age' eeps' masses' mags'
        repackMass v = V.map (MkMass . nonNegative') v
        repackMags fs v =
          let filterSets = map (V.map (MkMagnitude . packLog)) v
          in M.fromList $ zip fs filterSets


convertModels_Maps :: [(([ByteString], Double, Double), S.Set Age)] -> [(FeH, (M.Map HeliumFraction (S.Set Isochrone)))]
convertModels_Maps = map go
  where go ((fs, feh, y), isochrone) =
          let feh'  = MkFeH . packLog $ feh
              y'    = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
              iso'  = S.map (repackAge fs) isochrone
          in (feh', M.insert y' iso' mempty)
        repackAge fs (Age age eeps masses magnitudes) =
          let age'    = MkLogAge . packLog $ age
              eeps'   = V.map toEnum eeps
              masses' = repackMass masses
              mags'   = repackMags fs magnitudes
          in Isochrone age' eeps' masses' mags'
        repackMass v = V.map (MkMass . nonNegative') v
        repackMags fs v =
          let filterSets = map (V.map (MkMagnitude . packLog)) v
          in M.fromList $ zip fs filterSets
