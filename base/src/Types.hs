{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Types where

import Data.Maybe (fromJust)

import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny, suchThat)

import Numeric.MathFunctions.Comparison (addUlps)


{-@ type GT  N = {v:Double | v >  N} @-}
{-@ type GTE N = {v:Double | v >= N} @-}
{-@ type LT  N = {v:Double | v <  N} @-}
{-@ type LTE N = {v:Double | v <= N} @-}
{-@ type Btwn LO HI = {v:Double | LO <= v && v <= HI} @-}

{-@ assume abs :: _ -> {v:_ | 0 <= v} @-}
{-@ assume choose :: System.Random.Random a => t:(a, a) -> Test.QuickCheck.Gen {v:a | v >= fst t && v <= snd t} @-}
{-@ assume addUlps :: {u:Int | u > 0} -> v:Double -> {r:Double | r > v} @-}

{-@ newtype Percentage = Percentage (Btwn 0 1) @-}
newtype Percentage = Percentage Double
                   deriving (Show)

instance Arbitrary Percentage where
  arbitrary = choose (0.0, 1.0) >>= return . Percentage


percentage :: Double -> Maybe Percentage
percentage f | f >= 0 && f <= 1.0 = Just $ Percentage f
             | otherwise          = Nothing


{-@ percentage' :: GT 0 -> Percentage @-}
percentage' :: Double -> Percentage
percentage' = fromJust . percentage


{-@ newtype Positive = MkPositive (GT 0) @-}
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


positive' :: Double -> Positive
positive' = fromJust . positive


{-@ newtype NonNegative = MkNonNegative { unNonNegative :: GTE 0 } @-}
newtype NonNegative = MkNonNegative { unNonNegative :: Double }

instance Arbitrary NonNegative where
  arbitrary = MkNonNegative <$> (fmap abs arbitrary `suchThat` (>= 0))

nonNegative :: Double -> Maybe NonNegative
nonNegative f | f >= 0    = Just $ MkNonNegative f
              | otherwise = Nothing

{-@ nonNegative' :: GTE 0 -> NonNegative @-}
nonNegative' :: Double -> NonNegative
nonNegative' f = if f >= 0
                    then MkNonNegative f
                    else error "Negative value in nonNegative'"
