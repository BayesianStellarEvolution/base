module Main where

import Data.Semigroup ((<>))

def :: [a]
def = undefined


clusterOptions :: Monoid a => a
clusterOptions = undefined


main :: IO ()
main = do options <- loadOptions (clusterOptions <> def)
          models <- loadModels (model options)
          undefined


loadOptions :: Monoid a => a -> b
loadOptions _ = undefined


loadModels :: String -> a
loadModels _ = undefined


model :: a -> b
model _ = undefined
