import Criterion.Main

import qualified Data.Map.Strict as M

import Interpolate
import Models.Input
import Types
import Types.Internal

-- Our benchmark harness.
main = do
  model <- loadModels NewDsed
  let i1 = snd . M.findMin . snd . M.findMin . snd . M.findMin . convertModels $ model
      i2 = snd . M.findMax . snd . M.findMax . snd . M.findMin . convertModels $ model

  defaultMain [
    bench "convertModels" $ whnf convertModels model,
    bench "interpolateIsochrones" $ whnf (interpolateIsochrones (closedUnitInterval' 0.5) i1) i2,
    bgroup "convertModels" []]
