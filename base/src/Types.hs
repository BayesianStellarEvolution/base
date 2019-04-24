module Types where

import Data.Maybe (fromJust)

import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny)


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


{-@ type NonNegativeDouble = {v:Double | 0 <= v} @-}
{-@ newtype NonNegativeDouble = NonNegativeDouble NonNegativeDouble @-}
newtype NonNegativeDouble = NonNegativeDouble Double
                       deriving (Show)

instance Arbitrary NonNegativeDouble where
  arbitrary = NonNegativeDouble . abs <$> chooseAny


nonNegative :: Double -> Maybe NonNegativeDouble
nonNegative f | f >= 0    = Just $ NonNegativeDouble f
              | otherwise = Nothing


nonNegative' :: Double -> NonNegativeDouble
nonNegative' = fromJust . nonNegative
