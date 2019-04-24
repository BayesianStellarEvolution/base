{-# LANGUAGE GADTs, StandaloneDeriving #-}
module Types where

import Data.Maybe (fromJust)

import qualified Test.QuickCheck as Q
import Test.QuickCheck     (Arbitrary (..))
import Test.QuickCheck.Gen (choose, chooseAny, suchThat)

{-@ type GT  N = {v:_ | v >  N} @-}
{-@ type GTE N = {v:_ | v >= N} @-}
{-@ type LT  N = {v:_ | v <  N} @-}
{-@ type LTE N = {v:_ | v <= N} @-}
{-@ type BTWN LO HI = {v:_ | LO <= v && v <= HI} @-}

{-@ assume abs :: _ -> {v:_ | 0 <= v} @-}
{-@ assume choose :: System.Random.Random a => t:(a, a) -> Test.QuickCheck.Gen {v:a | v >= fst t && v <= snd t} @-}

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



{-@ data NonNegative a where
      MkNonNegative :: GTE 0 -> NonNegative a @-}
data NonNegative a where
  MkNonNegative :: Num a => !a -> NonNegative a

deriving instance Eq a => Eq (NonNegative a)
deriving instance Ord a => Ord (NonNegative a)

{-@ measure unNonNegative @-}
unNonNegative :: NonNegative a -> a
unNonNegative (MkNonNegative a) = a

instance (Ord a, Num a) => Num (NonNegative a) where
  (+) a b = MkNonNegative (unNonNegative a + unNonNegative b)
  (*) a b = MkNonNegative (unNonNegative a * unNonNegative b)
  negate = error "Can't negate a non-negative"
  abs = id
  signum a = MkNonNegative (signum (unNonNegative a))
  fromInteger a = MkNonNegative (fromInteger a)

instance (Ord a, Num a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary = MkNonNegative <$> (fmap abs arbitrary `suchThat` (>= 0))

nonNegative :: (Ord a, Num a) => a -> Maybe (NonNegative a)
nonNegative f | f >= 0    = Just $ MkNonNegative f
              | otherwise = Nothing

{-@ nonNegative' :: (Ord a, Num a) => GTE 0 -> NonNegative a @-}
nonNegative' :: (Ord a, Num a) => a -> (NonNegative a)
nonNegative' f = if f >= 0
                    then MkNonNegative f
                    else error "Negative value in nonNegative'"
