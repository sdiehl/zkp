{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Verify
  ( runVerify,
  )
where

import Data.Aeson
import qualified Data.Curve.Weierstrass.BN254 as G1
import qualified Data.Curve.Weierstrass.BN254T as G2
import Data.Field.Galois (Extension)
import Data.Pairing.BN254 (BN254, Fr, Pairing (..))
import qualified Protocol.Groth as Groth
import Protolude
import System.FilePath.Posix

deriving instance FromJSON G1.PA

deriving instance FromJSON G2.PA

deriving instance FromJSON (Extension G2.U G1.Fq)

runVerify :: FilePath -> FilePath -> FilePath -> IO ()
runVerify setupDir inputFile piFile = do
  Just (reference :: Groth.Reference (G1 BN254) (G2 BN254)) <- decodeFileStrict (setupDir </> "ref")
  Just (inputs :: Map Int Fr) <- decodeFileStrict inputFile
  Just (pi :: Groth.Proof (G1 BN254) (G2 BN254)) <- decodeFileStrict piFile
  case Groth.verify (Groth.refV reference) inputs pi of
    True -> do
      putText "Proof valid"
      exitSuccess
    False -> do
      putText "Proof invalid"
      exitFailure
