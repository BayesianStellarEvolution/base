{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell #-}
module Types (module Types.Magnitude
             ,TotalAge (..)
             ,CoolingAge (..)
             ,FeH (..)
             ,HeliumFraction (..)
             ,LogAge (..)
             ,Mass (..)
             ,Isochrone (..)
             ,Cluster (..)) where

import Types.Internal
import Types.Magnitude

import qualified Data.Map.Strict as M
import Data.Text
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving


newtype FeH = MkFeH { unFeH :: Log10 }
        deriving (Read, Show, Eq, Ord)


newtype HeliumFraction = MkHeliumFraction { unHeliumFraction :: Percentage }
        deriving (Read, Show, Eq, Ord)


newtype Parallax = MkParallax { unParallax :: NonNegative }
        deriving (Show, Eq, Ord)


newtype CarbonFraction = MkCarbonFraction { unCarbonFraction :: Percentage }
        deriving (Show, Eq, Ord)

newtype Mass = MkMass { unMass :: NonNegative }
        deriving (Show, Eq, Ord)

derivingUnbox "Mass"
  [t| Mass -> NonNegative |]
  [| unMass |]
  [| MkMass |]

newtype Likelihood = MkLikelihood { unLikelihood :: ClosedUnitInterval }
        deriving (Show, Eq, Ord)


type EEP = Word
type Filter = Text

data Isochrone = Isochrone (V.Vector EEP) (V.Vector Mass) (M.Map Filter (V.Vector AbsoluteMagnitude))
          deriving (Eq, Show)


newtype LogAge = MkLogAge { unLogAge :: Log10 }
        deriving (Read, Show, Eq, Ord)


newtype TotalAge   = MkTotalAge   {   unTotalAge :: LogAge }
newtype CoolingAge = MkCoolingAge { unCoolingAge :: LogAge }


-- TODO: This should probably move?
data Cluster = Cluster { feh :: FeH, heliumFraction :: HeliumFraction, logAge :: LogAge }
     deriving (Read, Show)
