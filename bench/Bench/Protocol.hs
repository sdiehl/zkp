{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.Protocol where

import Bench.Reference
import Circuit.Arithmetic
import Criterion.Main
import Data.Curve.Weierstrass.BN254 (BN254)
import Data.Pairing.BN254 (Pairing (..), getRootOfUnity)
import Fresh
import qualified Protocol.Groth as Groth
import Protolude hiding (witness)
import QAP (arithCircuitToGenQAP, createPolynomials)
import Test.Arithmetic (ArithCircuitWithInput (..))

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "Groth"
      [ groth smallCircuitSize,
        groth mediumCircuitSize,
        groth largeCircuitSize
      ]
  ]

groth :: CircuitParams -> Benchmark
groth circuitSize =
  env (genCircuit circuitSize) $
    \(~(ArithCircuitWithInput program@(ArithCircuit gates) input)) ->
      let roots = evalFresh $ generateRoots (fromIntegral <$> fresh) program
          qap = createPolynomials getRootOfUnity $ arithCircuitToGenQAP roots program
          testReference = fst $ Groth.setup grothRandomSetup qap
          proof = Groth.prove grothRandomProver program qap (Groth.refP testReference) input
       in bgroup
            (show circuitSize)
            [ bench "random setup" $
                nf (Groth.setup grothRandomSetup) qap,
              bench "generate proof" $
                nf (Groth.prove grothRandomProver program qap (Groth.refP testReference)) input,
              bench "verify proof" $
                nf (Groth.verify (Groth.refV testReference) input) proof
            ]
