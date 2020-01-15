{-# LANGUAGE NoImplicitPrelude #-}

-- To get the benchmarking data, run "stack bench".

module Bench where

import qualified Bench.Circuit as Circuit
import qualified Bench.Poly as Poly
import qualified Bench.Protocol as Protocol
import Criterion.Main
import Protolude

main :: IO ()
main =
  defaultMain
    [ bgroup "Circuit to QAP translation" Circuit.benchmarks,
      bgroup "Polynomial operations" Poly.benchmarks,
      bgroup "Protocols" Protocol.benchmarks
    ]
