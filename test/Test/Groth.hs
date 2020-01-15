module Test.Groth where

import Circuit.Affine
import Circuit.Arithmetic
import Data.Curve.Weierstrass.BN254 (BN254, Fr)
import Data.Pairing (Pairing (..))
import Fresh
import Protocol.Groth
import Protolude
import QAP
import Test.Arithmetic
import qualified Test.QuickCheck.Monadic as QCM
import Test.Tasty.QuickCheck

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

-- Example 1: Case with intermediate wire
-- a - input wire
-- i - intermediate wire
-- o - output wire
--          o0 |
--            [X]
--         i0 / \
--          [X]  a2
--          / \
--        a0   a1
testArithCircuit1 :: ArithCircuit Fr
testArithCircuit1 =
  ArithCircuit
    [ Mul (Var (InputWire 0)) (Var (InputWire 1)) (IntermediateWire 0),
      Mul (Add (ConstGate 1) (Var (IntermediateWire 0))) (ScalarMul 7 (Var (InputWire 2))) (OutputWire 0)
    ]

prop_example_1_rnd_inputs ::
  RandomSetup Fr ->
  RandomProver Fr ->
  Property
prop_example_1_rnd_inputs rndSetup rndProver =
  withMaxSuccess 20 $ QCM.monadicIO $ do
    inputs <- lift . generate $ arbInputVector 3
    lift . pure $ testVerification inputs
  where
    roots = evalFresh $ generateRoots (fromIntegral <$> fresh) testArithCircuit1
    qap = arithCircuitToQAP roots testArithCircuit1
    testReference = fst $ setup rndSetup qap
    testProof :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
    testProof = prove rndProver testArithCircuit1 qap (refP testReference)
    testVerification :: Map Int Fr -> Bool
    testVerification input = verify (refV testReference) input (testProof input)

prop_example_1_rnd_inputs_simulation ::
  RandomSetup Fr ->
  RandomSimulator Fr ->
  Property
prop_example_1_rnd_inputs_simulation rndSetup rndSimulator =
  withMaxSuccess 5 $ QCM.monadicIO $ do
    inputs <- lift . generate $ arbInputVector 3
    lift . pure $ testAdversary inputs
  where
    roots = evalFresh $ generateRoots (fromIntegral <$> fresh) testArithCircuit1
    qap = arithCircuitToQAP roots testArithCircuit1
    testReference :: Reference (G1 BN254) (G2 BN254)
    testTrapdoor :: Trapdoor Fr
    (testReference, testTrapdoor) = setup rndSetup qap
    testSimulation :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
    testSimulation = simulate rndSimulator qap testTrapdoor
    testAdversary :: Map Int Fr -> Bool
    testAdversary i = verify (refV testReference) i (testSimulation i)

-- Example 2: Case with affine circuits and multiplication gates
--   o0 |    | o1
--     [X]  [X]
--    /   \ / \ +1
--  [+]   [-]  i3
--  / \x7 / \
-- i0   i1   i2
testArithCircuit2 :: ArithCircuit Fr
testArithCircuit2 =
  ArithCircuit
    [ Mul
        (Add (Var (InputWire 0)) (ScalarMul 7 (Var (InputWire 1))))
        (Add (Var (InputWire 1)) (ScalarMul (-1) (Var (InputWire 2))))
        (OutputWire 0),
      Mul
        (Add (Var (InputWire 1)) (ScalarMul (-1) (Var (InputWire 2))))
        (Add (Var (InputWire 3)) (ConstGate 1))
        (OutputWire 1)
    ]

prop_example_2_rnd_inputs ::
  RandomSetup Fr ->
  RandomProver Fr ->
  Property
prop_example_2_rnd_inputs rndSetup rndProver =
  withMaxSuccess 20 $ QCM.monadicIO $ do
    inputs <- lift . generate $ arbInputVector 4
    lift . pure $ testVerification inputs
  where
    roots = evalFresh $ generateRoots (fromIntegral <$> fresh) testArithCircuit2
    qap = arithCircuitToQAP roots testArithCircuit2
    testReference = fst $ setup rndSetup qap
    testProof :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
    testProof = prove rndProver testArithCircuit2 qap (refP testReference)
    testVerification :: Map Int Fr -> Bool
    testVerification i = verify (refV testReference) i (testProof i)

prop_example_2_rnd_inputs_simulation ::
  RandomSetup Fr ->
  RandomSimulator Fr ->
  Property
prop_example_2_rnd_inputs_simulation rndSetup rndSimulator =
  withMaxSuccess 5 $ QCM.monadicIO $ do
    inputs <- lift . generate $ arbInputVector 4
    lift . pure $ testAdversary inputs
  where
    roots = evalFresh $ generateRoots (fromIntegral <$> fresh) testArithCircuit2
    qap = arithCircuitToQAP roots testArithCircuit2
    (testReference, testTrapdoor) = setup rndSetup qap
    testSimulation :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
    testSimulation = simulate rndSimulator qap testTrapdoor
    testAdversary :: Map Int Fr -> Bool
    testAdversary i = verify (refV testReference) i (testSimulation i)

prop_rnd_arithmetic_circuit ::
  ArithCircuitWithInputs Fr ->
  RandomSetup Fr ->
  RandomProver Fr ->
  Property
prop_rnd_arithmetic_circuit
  (ArithCircuitWithInputs program@(ArithCircuit gates) inputs)
  rndSetup
  rndProver =
    withMaxSuccess 10 $ case gates of
      [] -> True
      (_ : _) -> all testVerification inputs
    where
      roots :: [[Fr]]
      roots = evalFresh $ generateRoots (fromIntegral <$> fresh) program
      qap = arithCircuitToQAP roots program
      testReference = fst $ setup rndSetup qap
      testProof :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
      testProof = prove rndProver program qap (refP testReference)
      testVerification :: Map Int Fr -> Bool
      testVerification input = verify (refV testReference) input (testProof input)

prop_rnd_arithmetic_circuit_simulation ::
  ArithCircuitWithInputs Fr ->
  RandomSetup Fr ->
  RandomSimulator Fr ->
  Property
prop_rnd_arithmetic_circuit_simulation
  (ArithCircuitWithInputs program@(ArithCircuit gates) inputs)
  rndSetup
  rndSimulator =
    withMaxSuccess 5 $ case gates of
      [] -> True
      (_ : _) -> all testAdversary inputs
    where
      roots :: [[Fr]]
      roots = evalFresh $ generateRoots (fromIntegral <$> fresh) program
      qap = arithCircuitToQAP roots program
      testTrapdoor :: Trapdoor Fr
      (testReference, testTrapdoor) = setup rndSetup qap
      testSimulation :: Map Int Fr -> Proof (G1 BN254) (G2 BN254)
      testSimulation = simulate rndSimulator qap testTrapdoor
      testAdversary :: Map Int Fr -> Bool
      testAdversary i = verify (refV testReference) i (testSimulation i)
