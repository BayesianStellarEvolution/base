module Paths where


data MSModel = OldDSED | NewDSED | Yale2018 | PARSEC
             deriving (Read, Show)

class HasModelPath a where
  modelPath :: a -> FilePath -> FilePath

instance HasModelPath MSModel where
  modelPath OldDSED  base = base ++ "mainSequence/dsed_old.model.xz"
  modelPath NewDSED  base = base ++ "mainSequence/dsed_new.model.xz"
  modelPath PARSEC   base = base ++ "mainSequence/PARSEC.model.xz"
  modelPath Yale2018 base = base ++ "mainSequence/yale_2018.model.xz"
