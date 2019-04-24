{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Types where

import Data.Maybe (fromJust)

import qualified Test.QuickCheck as Q
import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny, suchThat)

{-@ type GT  N = {v:Double | v >  N} @-}
{-@ type GTE N = {v:Double | v >= N} @-}
{-@ type LT  N = {v:Double | v <  N} @-}
{-@ type LTE N = {v:Double | v <= N} @-}
{-@ type Btwn LO HI = {v:Double | LO <= v && v <= HI} @-}

{-@ assume abs :: _ -> {v:_ | 0 <= v} @-}
{-@ assume choose :: System.Random.Random a => t:(a, a) -> Test.QuickCheck.Gen {v:a | v >= fst t && v <= snd t} @-}

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


{-@ newtype PositiveDouble = PositiveDouble (GT 0) @-}
newtype PositiveDouble = PositiveDouble Double
                       deriving (Show)

instance Arbitrary PositiveDouble where
  arbitrary = PositiveDouble . abs <$> chooseAny


positive :: Double -> Maybe PositiveDouble
positive f | f >= 0    = Just $ PositiveDouble f
           | otherwise = Nothing


positive' :: Double -> PositiveDouble
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
