{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Curve
  ( Curve (Point),
    mul,
  )
where

import Data.Field.Galois (GaloisField, PrimeField)
import GHC.Natural (Natural)

-- | Curve coordinates.
data Coordinates
  = Affine
  | Jacobian
  | Projective

-- | Curve forms.
data Form
  = Binary
  | Edwards
  | Montgomery
  | Weierstrass

class
  ( GaloisField q,
    PrimeField r,
    Eq (Point f c e q r),
    Show (Point f c e q r)
  ) =>
  Curve (f :: Form) (c :: Coordinates) e q r where
  -- | Curve point.
  data Point f c e q r :: *

mul = undefined
