module Options where

import Cluster
import Paths

import Options.Applicative
import Data.Semigroup ((<>))

loadOptions :: Monoid a => a -> b
loadOptions _ = undefined


model :: a -> MSModel
model _ = undefined


cluster :: a -> Cluster
cluster _ = undefined
