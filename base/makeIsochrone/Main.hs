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

import MainSequenceModel

main :: IO ()
main = do
  t <- runConduitRes ( sourceFile "../../base-models/dsed/dsed_new.model.xz"
                     .| decompress Nothing
                     .| lexModel
                     .| parseModel
                     .| sinkList )

  let (feh, ages) = t !! 4
      go (Age a _ _ fs) =
        let b = fs !! 1
            v = fs !! 2
            bmv = V.zipWith (-) b v
        in plot $ do
          pts <- points (show a) $ V.toList $ V.zip bmv v
          return $ pts & plot_points_style . point_radius .~ 1

  toFile (def & (fo_size .~ (1920, 1440)) & (fo_format .~ SVG)) "/tmp/test.svg" $ do
    setColors (map opaque [blue, green, purple, yellow, red])

    layout_title .= "New DSED, [Fe/H] = " ++ show feh
    layout_x_axis .= (def & laxis_title .~ "B - V")
    layout_y_axis .= (def & laxis_reverse .~ True & laxis_title .~ "V")
    mapM_ go $ S.elems ages
