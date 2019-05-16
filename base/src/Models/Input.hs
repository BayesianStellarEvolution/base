module Models.Input where

import Conduit

import Data.Conduit.Lzma
import Data.ByteString   (ByteString)
import Data.Set          (Set)

import qualified Data.Map as M
import qualified Data.Set as S (toList)
import qualified Data.Vector.Unboxed as V

import MainSequenceModel
import Paths
import Types


type RawModel = [(([ByteString], Double, Double), Set Age)]
type Model    = M.Map FeH (M.Map HeliumFraction (M.Map LogAge Isochrone))

loadModels :: (MonadThrow m, HasModelPath p, MonadUnliftIO m) => p -> m RawModel
loadModels model = runConduitRes $ loadModel .| sinkList
  where loadModel = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel


convertModels :: RawModel -> Model
convertModels = M.fromListWith (M.union) . map go
  where go ((filters, feh, y), isochrone) =
          let feh'  = MkFeH . packLog $ feh
              y'    = MkHeliumFraction . MkPercentage . closedUnitInterval' $ y
              iso'  = M.fromList . map (repackAge filters) . S.toList $ isochrone
          in (feh', M.insert y' iso' mempty)
        repackAge filters (Age age eeps masses magnitudes) =
          let age'    = MkLogAge . packLog $ age
              eeps'   = V.map toEnum eeps
              masses' = repackMass masses
              mags'   = repackMags filters magnitudes
          in (age', Isochrone eeps' masses' mags')
        repackMass v = V.map (MkMass . nonNegative') v
        repackMags filters v =
          let filterSets = map (V.map (MkMagnitude . packLog)) v
          in M.fromList $ zip filters filterSets
