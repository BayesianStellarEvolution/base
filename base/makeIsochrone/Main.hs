module Main where

def :: [a]
def = []


main :: IO ()
main = do options <- loadOptions def
          models <- loadModels (model options)
          undefined


loadOptions :: Monoid a => a -> b
loadOptions _ = undefined


loadModels :: String -> a
loadModels _ = undefined


model :: a -> b
model _ = undefined
