{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Bench.Reference where

import qualified Data.Curve.Weierstrass.BN254 as BN254
import Data.Curve.Weierstrass.BN254 (Fr)
import qualified Data.Curve.Weierstrass.SECP256K1 as SECP256K1
import Protocol.Groth (RandomProver (..), RandomSetup (..))
import Protolude
import Test.Arithmetic (ArithCircuitWithInput (..), arbArithCircuit, arbInputVector)
import Test.Tasty.QuickCheck

data CircuitParams
  = CircuitParams
      { numVars :: Int,
        mul :: Int,
        size :: Int
      }
  deriving (Show, Generic, NFData)

smallCircuitSize :: CircuitParams
smallCircuitSize = CircuitParams 10 10 10

mediumCircuitSize :: CircuitParams
mediumCircuitSize = CircuitParams 100 100 100

largeCircuitSize :: CircuitParams
largeCircuitSize = CircuitParams 100 10000 100

genCircuit :: CircuitParams -> IO (ArithCircuitWithInput BN254.Fr)
genCircuit CircuitParams {..} = generate $ do
  program <- arbArithCircuit (mul, 0, 0) numVars size
  input <- arbInputVector numVars
  pure $ ArithCircuitWithInput program input

grothRandomSetup :: RandomSetup Fr
grothRandomSetup = RandomSetup 2 3 4 5 6

grothRandomProver :: RandomProver Fr
grothRandomProver = RandomProver 7 8
