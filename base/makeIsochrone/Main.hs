module Main where

import Data.Semigroup ((<>))

def :: [a]
def = undefined


clusterOptions :: Monoid a => a
clusterOptions = undefined


main :: IO ()
main = do options <- loadOptions (clusterOptions <> def)
          models <- loadModels (model options)
          let interpolated = interpolateIsochrone model (cluster options)
          undefined


loadOptions :: Monoid a => a -> b
loadOptions _ = undefined


loadModels :: String -> a
loadModels _ = undefined


model :: a -> b
model _ = undefined


cluster :: a -> b
cluster _ = undefined

interpolateIsochrone :: p1 -> p2 -> a
interpolateIsochrone model cluster = undefined
