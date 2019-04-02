module Main where

import Data.Semigroup ((<>))

def :: [a]
def = undefined


clusterOptions :: Monoid a => a
clusterOptions = undefined


data InterpolationMethod = Nearest


main :: IO ()
main = do options <- loadOptions (clusterOptions <> def)
          models <- loadModels (model options)
          let interpolated = interpolateIsochrone Nearest model (cluster options)
          withDB "db.base" $ \db -> storePhotometry interpolated


loadOptions :: Monoid a => a -> b
loadOptions _ = undefined


loadModels :: String -> a
loadModels _ = undefined


model :: a -> b
model _ = undefined


cluster :: a -> b
cluster _ = undefined


withDB :: String -> (a -> b) -> b
withDB _ _ = undefined


interpolateIsochrone :: p1 -> p2 -> p3 -> a
interpolateIsochrone _ _ _ = undefined


storePhotometry :: p -> a
storePhotometry _ = undefined
