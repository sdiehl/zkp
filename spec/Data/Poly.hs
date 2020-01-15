module Data.Poly where

newtype Poly v a = Poly {unPoly :: v a}
  deriving (Eq, Show)

data Vector a = Vector
  deriving (Eq, Show)

type VPoly = Poly Vector

eval = undefined

monomial = undefined
