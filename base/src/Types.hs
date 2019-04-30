{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Types where

import Data.Coerce (coerce)
import Data.Maybe (fromJust)

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
{-@ assume log :: Floating a => a -> {v:a | v >= 0} @-}
{-@ assume logBase :: Floating a => a -> a -> {v:a | v >= 0} @-}

{-@ type ClosedUnitIntervalR = Btwn 0 1 @-}


{-@ newtype ClosedUnitInterval = MkClosedUnitInterval { unClosedUnitInterval :: {v:Double | (v >= 0.0) && (v <= 1.0)} } @-}
newtype ClosedUnitInterval = MkClosedUnitInterval { unClosedUnitInterval :: Double }
                     deriving (Show)

instance Arbitrary ClosedUnitInterval where
  arbitrary = choose (0, 1) >>= return . MkClosedUnitInterval


closedUnitInterval :: Double -> Maybe ClosedUnitInterval
closedUnitInterval f | f >= 0 && f <= 1 = Just $ MkClosedUnitInterval f
                     | otherwise        = Nothing


{-@ closedUnitInterval' :: ClosedUnitIntervalR -> ClosedUnitInterval @-}
closedUnitInterval' :: Double -> ClosedUnitInterval
closedUnitInterval' f = if 0 <= f && f <= 1
                           then MkClosedUnitInterval f
                           else error "Out of bounds in closedUnitInterval'"




{-@ newtype Positive = MkPositive ( unPositive :: GT 0) @-}
newtype Positive = MkPositive { unPositive :: Double }
                   deriving (Show)

instance Arbitrary Positive where
  arbitrary = do val <- abs <$> chooseAny
                 return $ if val == 0
                             then MkPositive (addUlps 1 val)
                             else MkPositive val


positive :: Double -> Maybe Positive
positive f | f > 0     = Just $ MkPositive f
           | otherwise = Nothing


{-@ positive' :: GT 0 -> Positive @-}
positive' :: Double -> Positive
positive' f = if f > 0
                 then MkPositive f
                 else error "Negative value in nonNegative'"




{-@ newtype NonNegative = MkNonNegative { unNonNegative :: GTE 0 } @-}
newtype NonNegative = MkNonNegative { unNonNegative :: Double }

instance Arbitrary NonNegative where
  arbitrary = MkNonNegative <$> (fmap abs arbitrary `suchThat` (> 0))


nonNegative :: Double -> Maybe NonNegative
nonNegative f | f >= 0    = Just $ MkNonNegative f
              | otherwise = Nothing


{-@ nonNegative' :: GTE 0 -> NonNegative @-}
nonNegative' :: Double -> NonNegative
nonNegative' f = if f >= 0
                    then MkNonNegative f
                    else error "Negative value in nonNegative'"




newtype NaturalLog = MkNaturalLog { unNaturalLog :: Double }
             deriving (Show)

toNaturalLogSpace :: NonNegative -> NaturalLog
toNaturalLogSpace = MkNaturalLog . exp . coerce

fromNaturalLogSpace :: NaturalLog -> NonNegative
fromNaturalLogSpace = nonNegative' . log . coerce




newtype Log10 = MkLog10 { unLog10 :: Double }
             deriving (Show)

toLog10Space :: NonNegative -> Log10
toLog10Space = MkLog10 . (10 **) . coerce

fromLog10Space :: Log10 -> NonNegative
fromLog10Space = nonNegative' . logBase 10 . coerce




newtype Log2 = MkLog2 { unLog2 :: Double }
             deriving (Show)

toLog2Space :: NonNegative -> Log2
toLog2Space = MkLog2 . (2 **) . coerce

fromLog2Space :: Log2 -> NonNegative
fromLog2Space = nonNegative' . logBase 2 . coerce
