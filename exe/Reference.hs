module Reference where

import Circuit.Arithmetic (ArithCircuit)
import Circuit.Expr (execCircuitBuilder)
import Circuit.Lang
import Data.Curve.Weierstrass.BN254 (Fr)
import qualified Data.Map as Map
import Protolude

-- Small example program whose executions we want to verify.
exampleProgram1 :: ArithCircuit Fr
exampleProgram1 =
  execCircuitBuilder $ do
    -- We need to register the "wires" we use for inputs
    x <- input
    y <- input
    -- Intermediate results of an expression can be "stored" on an
    -- intermediate wire.
    z <- e $ deref x `mul` deref x
    w <- e $ deref x `add` c 1
    -- With "ret", we can route an expression to a fresh output
    -- variable.
    ret $ cond (deref y `eq` (deref z `add` deref w)) (c 0) (c 1)

exampleInputs1 :: Map Int Fr
exampleInputs1 = Map.fromList [(3, 13), (4, 21)]

exampleProgram2 :: ArithCircuit Fr
exampleProgram2 =
  execCircuitBuilder $ do
    -- We need to register the "wires" we use for inputs
    x <- input
    y <- input
    -- Intermediate results of an expression can be "stored" on an
    -- intermediate wire.
    z <- e $ deref x `mul` deref x `mul` deref x
    w <- e $ deref x `add` c 10
    -- With "ret", we can route an expression to a fresh output
    -- variable.
    ret $ cond (deref y `eq` (deref z `add` deref w)) (c 0) (c 1)

exampleInputs2 :: Map Int Fr
exampleInputs2 = Map.fromList [(3, 40), (4, 78)]

-- Small example program whose executions we want to verify.
exampleProgram3 :: ArithCircuit Fr
exampleProgram3 =
  execCircuitBuilder $ do
    -- We need to register the "wires" we use for inputs
    x <- input
    y <- input
    -- Intermediate results of an expression can be "stored" on an
    -- intermediate wire.
    z <- e $ deref x `add` deref y
    w <-
      e $
        cond
          (deref z `eq` c 10)
          (c 20 `mul` deref z)
          (c 30 `mul` deref z)
    -- With "ret", we can route an expression to a fresh output
    -- variable.
    ret $ deref z `mul` deref w

exampleInputs3 :: Map Int Fr
exampleInputs3 = Map.fromList [(0, 123), (1, 456)]
