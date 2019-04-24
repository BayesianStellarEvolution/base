{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Maybe (fromJust)

import qualified Test.QuickCheck as Q
import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny, suchThat)


{-@ assume abs :: _ -> {v:_ | 0 <= v} @-}

{-@ type Percentage = {v:Double | 0 <= v && 1 >= v} @-}
{-@ newtype Percentage = Percentage Percentage @-}
newtype Percentage = Percentage Double
                   deriving (Show)

instance Arbitrary Percentage where
  arbitrary = choose (0.0, 1.0) >>= \val -> if val >= 0.0 && val <= 1.0
                                             then return $ Percentage val
                                             else error "Should never happen"


percentage :: Double -> Maybe Percentage
percentage f | f >= 0 && f <= 1.0 = Just $ Percentage f
             | otherwise          = Nothing


percentage' :: Double -> Percentage
percentage' = fromJust . percentage


{-@ type PositiveDouble = {v:Double | 0 <= v} @-}
{-@ newtype PositiveDouble = PositiveDouble PositiveDouble @-}
newtype PositiveDouble = PositiveDouble Double
                       deriving (Show)

instance Arbitrary PositiveDouble where
  arbitrary = PositiveDouble . abs <$> chooseAny


positive :: Double -> Maybe PositiveDouble
positive f | f >= 0    = Just $ PositiveDouble f
           | otherwise = Nothing


positive' :: Double -> PositiveDouble
positive' = fromJust . positive


{-@ type NonNegative = {v:Double | 0 <= v} @-}
{-@ newtype NonNegative a = MkNonNegative {getNonNegative :: NonNegative} @-}
newtype NonNegative a = MkNonNegative {getNonNegative :: a}
                    deriving (Ord, Num, Eq, Enum, Show, Read)

instance (Ord a, Num a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = MkNonNegative <$> (fmap abs arbitrary `suchThat` (>= 0))


nonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
nonNegative f | f >= 0    = Just $ MkNonNegative f
              | otherwise = Nothing


nonNegative' :: (Ord a, Num a) => a -> (NonNegative a)
nonNegative' = fromJust . nonNegative
