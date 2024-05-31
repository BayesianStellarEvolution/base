{-# LANGUAGE TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
module Types.Magnitude (AbsoluteMagnitude(..)) where

import Types.Internal
import Data.Vector.Unboxed.Deriving


newtype AbsoluteMagnitude = MkAbsoluteMagnitude { unAbsoluteMagnitude :: Log10 }
        deriving (Show, Eq, Ord)

derivingUnbox "AbsoluteMagnitude"
  [t| AbsoluteMagnitude -> Log10 |]
  [| unAbsoluteMagnitude |]
  [| MkAbsoluteMagnitude |]

newtype ApparentMagnitude = MkApparentMagniutude { unApparentMagnitude :: Log10 }
        deriving (Show, Eq, Ord)

