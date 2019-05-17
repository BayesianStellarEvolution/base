module Paths where


data MSModel = OldDsed | NewDsed | Yale2018 | Parsec
             deriving (Read, Show)

class HasModelPath a where
  modelPath :: a -> FilePath -> FilePath

instance HasModelPath MSModel where
  modelPath OldDsed  base = base ++ "mainSequence/dsed_old.model.xz"
  modelPath NewDsed  base = base ++ "mainSequence/dsed_new.model.xz"
  modelPath Parsec   base = base ++ "mainSequence/PARSEC.model.xz"
  modelPath Yale2018 base = base ++ "mainSequence/yale_2018.model.xz"
