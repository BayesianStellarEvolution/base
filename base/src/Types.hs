{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Types where

import Control.Exception (Exception, throw)

import Data.Coerce (coerce)
import Data.ByteString   (ByteString)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Deriving

import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny, suchThat)

import Numeric.MathFunctions.Comparison (addUlps)


{-@ type GT  N = {v:Double | v >  N} @-}
{-@ type GTE N = {v:Double | v >= N} @-}
{-@ type LT  N = {v:Double | v <  N} @-}
{-@ type LTE N = {v:Double | v <= N} @-}
{-@ type Btwn LO HI = {v:Double | (LO <= v) && (v <= HI)} @-}


{-@ assume abs :: __-> {v:_ | 0 <= v} @-}
{-@ assume choose :: System.Random.Random a => t:(a, a) -> Test.QuickCheck.Gen {v:a | (v >= fst t) && (v <= snd t)} @-}
{-@ assume addUlps :: {u:Int | u > 0} -> v:Double -> {r:Double | r > v} @-}
{-@ assume log :: Floating a => {v:a | v >= 0} -> a @-}
{-@ assume exp :: Floating a => a -> {v:a | v >= 0} @-}
{-@ assume logBase :: Floating a => {base:a | base >= 0} -> {v:a | v >= 0} -> a @-}
{-@ assume GHC.Float.** :: Floating a => {base:a | base >= 0} -> a -> {v:a | v >= 0} @-}
{-@ assume GHC.Float.pi :: Floating a => {v:a | v > 3.141592 && v < 3.141593} @-}

{-@ type ClosedUnitIntervalR = Btwn 0 1 @-}
{-@ newtype ClosedUnitInterval = MkClosedUnitInterval { unClosedUnitInterval :: ClosedUnitIntervalR} @-}
newtype ClosedUnitInterval = MkClosedUnitInterval { unClosedUnitInterval :: Double }
        deriving (Show, Eq, Ord)

instance Arbitrary ClosedUnitInterval where
  arbitrary = choose (0, 1) >>= return . MkClosedUnitInterval


data ClosedUnitIntervalBoundsException = ClosedUnitIntervalBoundsException
     deriving (Show)

instance Exception ClosedUnitIntervalBoundsException


closedUnitInterval :: Double -> Maybe ClosedUnitInterval
closedUnitInterval f | f >= 0 && f <= 1 = Just $ MkClosedUnitInterval f
                     | otherwise        = Nothing


closedUnitInterval' :: Double -> ClosedUnitInterval
closedUnitInterval' f = if 0 <= f && f <= 1
                           then MkClosedUnitInterval f
                           else throw ClosedUnitIntervalBoundsException


-- | This drops the refinement on the first parameter for circumstances where
-- it's non-trivial to prove the refinement. It should only be used by internal
-- code, if possible.
{-@ closedUnitInterval_unsafe :: ClosedUnitIntervalR -> ClosedUnitInterval @-}
closedUnitInterval_unsafe :: Double -> ClosedUnitInterval
closedUnitInterval_unsafe = MkClosedUnitInterval




{-@ type PositiveR = GT 0 @-}
{-@ newtype Positive = MkPositive ( unPositive :: PositiveR) @-}
newtype Positive = MkPositive { unPositive :: Double }
        deriving (Show, Eq, Ord)

instance Arbitrary Positive where
  arbitrary = do val <- abs <$> chooseAny
                 return $ if val == 0
                             then MkPositive (addUlps 1 val)
                             else MkPositive val

data PositiveBoundsException = PositiveBoundsException
     deriving (Show)

instance Exception PositiveBoundsException


positive :: Double -> Maybe Positive
positive f | f > 0     = Just $ MkPositive f
           | otherwise = Nothing


positive' :: Double -> Positive
positive' f = if f > 0
                 then MkPositive f
                 else throw PositiveBoundsException


{-@ positive_unsafe :: PositiveR -> Positive @-}
positive_unsafe :: Double -> Positive
positive_unsafe = MkPositive


{-@ type NonNegativeR = GTE 0 @-}
{-@ newtype NonNegative = MkNonNegative { unNonNegative :: NonNegativeR } @-}
newtype NonNegative = MkNonNegative { unNonNegative :: Double }
        deriving (Show, Eq, Ord)

instance Arbitrary NonNegative where
  arbitrary = MkNonNegative <$> (fmap abs arbitrary `suchThat` (> 0))

instance Num NonNegative where
  (+) (MkNonNegative a) (MkNonNegative b) = MkNonNegative $ a + b
  (-) (MkNonNegative a) (MkNonNegative b) = nonNegative'  $ a - b
  (*) (MkNonNegative a) (MkNonNegative b) = MkNonNegative $ a * b
  abs = id
  signum = const 0
  fromInteger = nonNegative' . realToFrac
  negate _ = throw NonNegativeBoundsException


data NonNegativeBoundsException = NonNegativeBoundsException
     deriving (Show)

instance Exception NonNegativeBoundsException


nonNegative :: Double -> Maybe NonNegative
nonNegative f | f >= 0    = Just $ MkNonNegative f
              | otherwise = Nothing


nonNegative' :: Double -> NonNegative
nonNegative' f = if f >= 0
                    then MkNonNegative f
                    else throw NonNegativeBoundsException


{-@ nonNegative_unsafe :: NonNegativeR -> NonNegative @-}
nonNegative_unsafe :: Double -> NonNegative
nonNegative_unsafe = MkNonNegative

derivingUnbox "NonNegative"
  [t| NonNegative -> Double |]
  [| unNonNegative |]
  [| nonNegative'  |]




class LogSpace a where
  toLogSpace   :: NonNegative -> a -- ^ Take any non-negative number and express it as a value in log_N space
  fromLogSpace :: a -> NonNegative -- ^ Return a number to non-log space
  packLog   :: Double -> a -- ^ Encode a value already in log space as a LogSpace a
  unpackLog :: a -> Double -- ^ Access the raw log value directly




newtype NaturalLog = MkNaturalLog { unNaturalLog :: Double }
        deriving (Show, Eq, Ord)

toNaturalLogSpace :: NonNegative -> NaturalLog
toNaturalLogSpace = MkNaturalLog . log . coerce

fromNaturalLogSpace :: NaturalLog -> NonNegative
fromNaturalLogSpace = nonNegative_unsafe . exp . coerce

instance LogSpace NaturalLog where
  toLogSpace   = toNaturalLogSpace
  fromLogSpace = fromNaturalLogSpace
  packLog = MkNaturalLog
  unpackLog = unNaturalLog

instance Arbitrary NaturalLog where
  arbitrary = toLogSpace <$> arbitrary




newtype Log10 = MkLog10 { unLog10 :: Double }
        deriving (Show, Eq, Ord)

toLog10Space :: NonNegative -> Log10
toLog10Space = MkLog10 . logBase 10 . coerce

fromLog10Space :: Log10 -> NonNegative
fromLog10Space = nonNegative_unsafe . (10 **) . coerce

instance LogSpace Log10 where
  toLogSpace   = toLog10Space
  fromLogSpace = fromLog10Space
  packLog = MkLog10
  unpackLog = unLog10

instance Arbitrary Log10 where
  arbitrary = toLogSpace <$> arbitrary

derivingUnbox "Log10"
  [t| Log10 -> Double |]
  [| unLog10 |]
  [| MkLog10 |]



newtype Log2 = MkLog2 { unLog2 :: Double }
        deriving (Show , Eq, Ord)

toLog2Space :: NonNegative -> Log2
toLog2Space = MkLog2 . logBase 2 . coerce

fromLog2Space :: Log2 -> NonNegative
fromLog2Space = nonNegative_unsafe . (2 **) . coerce

instance LogSpace Log2 where
  toLogSpace   = toLog2Space
  fromLogSpace = fromLog2Space
  packLog = MkLog2
  unpackLog = unLog2

instance Arbitrary Log2 where
  arbitrary = toLogSpace <$> arbitrary




newtype Percentage = MkPercentage { unPercentage :: ClosedUnitInterval }
        deriving (Show, Eq, Ord)


newtype FeH = MkFeH { unFeH :: Log10 }
        deriving (Show, Eq, Ord)


newtype LogAge = MkLogAge { unLogAge :: Log10 }
        deriving (Show, Eq, Ord)


newtype Magnitude = MkMagnitude { unMagnitude :: Log10 }
        deriving (Show, Eq, Ord)

derivingUnbox "Magnitude"
  [t| Magnitude -> Log10 |]
  [| unMagnitude |]
  [| MkMagnitude |]


newtype HeliumFraction = MkHeliumFraction { unHeliumFraction :: Percentage }
        deriving (Show, Eq, Ord)


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
type Filter = ByteString

data Isochrone = Isochrone (V.Vector EEP) (V.Vector Mass) (M.Map Filter (V.Vector Magnitude))
          deriving (Eq, Show)
