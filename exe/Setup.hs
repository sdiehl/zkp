{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}

module Setup
  ( runSetup,
  )
where

import Circuit.Arithmetic (generateRoots)
import Crypto.Number.Serialize
import Data.Aeson
import Data.Curve.Weierstrass.BN254 (Fr)
import qualified Data.Curve.Weierstrass.BN254 as G1
import qualified Data.Curve.Weierstrass.BN254T as G2
import Data.Field.Galois (Extension, rnd, toP)
import qualified Protocol.Groth as Groth
import Protolude
import qualified QAP
import System.Directory
import System.Entropy
import System.FilePath.Posix
import Text.Pretty.Simple

-------------------------------------------------------------------------------
-- Setup
-------------------------------------------------------------------------------

deriving instance ToJSON G1.PA

deriving instance ToJSON G2.PA

deriving instance ToJSON (Extension G2.U G1.Fq)

softwareEntropy :: IO Fr
softwareEntropy = do
  entropy <- getEntropy 128
  let result = toP (os2ip entropy) :: Fr
  pure result

hardwareEntropy :: IO Fr
hardwareEntropy = do
  entropy <- getHardwareEntropy 128
  case entropy of
    Nothing -> die "Cannot generate entropy from hardware source."
    Just result -> pure (toP (os2ip result))

runSetup :: Bool -> Bool -> FilePath -> FilePath -> IO ()
runSetup verbose useHardware circuitFile outputDir = do
  Just program <- decodeFileStrict circuitFile
  roots <- case useHardware of
    True -> generateRoots hardwareEntropy program
    False -> generateRoots softwareEntropy program
  let qap = QAP.arithCircuitToQAP roots program
  rndSetup <- Groth.generateRandomSetup rnd
  let (ref, trapdoor) = Groth.setup rndSetup qap
  createDirectoryIfMissing False outputDir
  when verbose $ do
    pPrint rndSetup
    pPrint ref
    pPrint qap
    pPrint roots
  encodeFile (outputDir </> "setup") (rndSetup :: Groth.RandomSetup Fr)
  encodeFile (outputDir </> "qap") qap
  encodeFile (outputDir </> "ref") ref
  encodeFile (outputDir </> "roots") roots
