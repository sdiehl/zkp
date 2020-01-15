{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.Pairing.BN254 where

import GHC.Natural

data BN254

newtype Fr = P Natural
  deriving (Num)

{-@ ignore Fr @-}
instance Fractional Fr

--instance Fractional Fr where
--  recip = undefined

class (Eq e) => Pairing e where

  {-# MINIMAL pairing #-}

  -- | Left group @G1@.
  type G1 e = (g :: *) | g -> e

  -- | Right group @G2@.
  type G2 e = (g :: *) | g -> e

  -- | Target group @GT@.
  type GT e = (g :: *) | g -> e

  -- | Computable non-degenerate bilinear map.
  pairing :: G1 e -> G2 e -> GT e
