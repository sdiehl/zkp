module Test.Poly where

import Data.Curve.Weierstrass
import Data.Curve.Weierstrass.BN254 (BN254, Fr)
import Data.Euclidean (Euclidean (..))
import Data.Pairing (Pairing (..))
import Data.Poly (VPoly (..), eval, gcdExt, monomial, toPoly)
import Data.Vector (fromList)
import Protocol.Groth (secretEvalInExponent)
import Protolude hiding (rem)
import Test.QuickCheck.Monadic (monadicIO)
import Test.Tasty.QuickCheck

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

-- | Generate a polynomial of given degree. For zero-degree
-- polynomials, we always return the zero polynomial. (This is because
-- that is the only zero-degree polynomial that the Chinese remainder
-- theorem works with.)
arbPolyOfDegree :: (Eq a, Num a, Arbitrary a) => Int -> Gen (VPoly a)
arbPolyOfDegree 0 = pure 0
arbPolyOfDegree deg = toPoly . fromList <$> vector deg

instance (Eq a, Num a, Arbitrary a) => Arbitrary (PolyEqnSystem a) where
  arbitrary = do
    degn1 <- choose (2, 10)
    degn2 <- choose (2, 10)
    dega1 <- choose (1, degn1 - 1)
    dega2 <- choose (1, degn2 - 1)
    n1 <- arbPolyOfDegree degn1
    n2 <- arbPolyOfDegree degn2
    a1 <- arbPolyOfDegree dega1
    a2 <- arbPolyOfDegree dega2
    pure $ PolyEqnSystem n1 n2 a1 a2

-- | System of polynomial equations
data PolyEqnSystem a = PolyEqnSystem (VPoly a) (VPoly a) (VPoly a) (VPoly a)
  deriving (Show)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

prop_secretEvalInExponentCorrect ::
  -- | degree
  Positive Int ->
  -- | secret value
  Fr ->
  Property
prop_secretEvalInExponentCorrect (Positive deg) s =
  monadicIO $ do
    p <- lift $ generate $ arbPolyOfDegree deg
    pure $ expectedValue p == secretEvalInExponent p powersOfS
  where
    g = gen :: G1 BN254
    expectedValue p = g `mul` (eval p s)
    powersOfS = map (\i -> g `mul` (s ^ i)) [0 .. deg]
