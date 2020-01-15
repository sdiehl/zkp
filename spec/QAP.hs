{-# LANGUAGE DeriveFunctor #-}

module QAP where

import Data.Map
import Data.Poly

data QapSet f
  = QapSet
      { qapSetConstant :: f,
        qapSetInput :: Map Int f,
        qapSetIntermediate :: Map Int f,
        qapSetOutput :: Map Int f
      }
  deriving (Show, Eq, Functor)

data QAP f
  = QAP
      { qapInputsLeft :: QapSet (VPoly f),
        qapInputsRight :: QapSet (VPoly f),
        qapOutputs :: QapSet (VPoly f),
        qapTarget :: VPoly f
      }
  deriving (Eq, Show)

sumQapSetCnstInp = undefined

sumQapSetMidOut = undefined

combineWithDefaults = undefined

cnstInpQapSet = undefined

combineNonInputsWithDefaults = undefined

combineInputsWithDefaults = undefined

generateAssignment = undefined
