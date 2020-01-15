module Test.QuickCheck (Arbitrary (..)) where

data Gen a

class Arbitrary a where
  arbitrary :: Gen a
