{-# LANGUAGE TypeApplications #-}
module Main where

import Conduit

import Control.Monad (when)

import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Conduit.Lzma
import Data.Either (isRight)
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import System.Environment

import MainSequenceModel
import Paths

main :: IO ()
main = do
  cliArgs <- read @MSModel . head <$> getArgs

  let loadModel model = sourceFile (modelPath model "models/") .| decompress Nothing .| lexModel .| parseModel

  models <- runConduitRes $ loadModel cliArgs .| sinkList
  mapM_ toFeHPlot models

toFeHPlot t = do
  let ((feh, y), ages) = t
      go (Age a _ _ fs) =
        let b = fs !! 1
            v = fs !! 2
            bmv = V.zipWith (-) b v
        in plot $ do
          pts <- points (show a) $ V.toList $ V.zip bmv v
          return $ pts & plot_points_style . point_radius .~ 1

  toFile (def & (fo_size .~ (7680, 5000)) -- (1920, 4000)) --1440))
              & (fo_format .~ PNG)
         ) ("/tmp/feh" ++ (if feh < 0 then 'm' else 'p'):show (abs feh) ++ "_y" ++ show y ++ ".png") $ do
    setColors (map opaque [blue, green, purple, yellow, red])

    layout_title .= "New DSED, [Fe/H] = " ++ show feh ++ ", Y = " ++ show y
    layout_x_axis .= (def & laxis_title .~ "B - V")
--                          & laxis_generate .~ scaledAxis def (-0.5, 2))
    layout_y_axis .= (def & laxis_reverse .~ True & laxis_title .~ "V"
                          & laxis_generate .~ scaledAxis def (-8, 15))
    mapM_ go $ S.elems ages
