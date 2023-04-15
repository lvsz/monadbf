{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BFPtr.Newtype (
      BFPtr(..)
    , (*+)
    , (*-)
    , ptrSubtract
    , ptrBounds
    ) where

import Data.Ix ( Ix )

ptrMax :: Integral a => a
ptrMax = 30000

newtype BFPtr = BFPtr Int deriving (Show, Eq, Ord, Ix, Integral, Real)
instance Num BFPtr where
    BFPtr a + BFPtr b = BFPtr ((a + b) `rem` ptrMax)
    BFPtr a * BFPtr b = BFPtr ((a * b) `rem` ptrMax)
    abs = id
    signum (BFPtr x) = BFPtr (signum x)
    fromInteger = BFPtr . fromInteger . flip rem ptrMax
    negate (BFPtr a) = BFPtr $ ptrMax - a
instance Bounded BFPtr where
    minBound = 0
    maxBound = pred ptrMax
instance Enum BFPtr where
    toEnum = BFPtr
    fromEnum (BFPtr x) = x
    succ = (+) 1
    pred = subtract 1

-- added to easily swap out the explicit dictionary version with this one
(*+) :: BFPtr -> BFPtr -> BFPtr
(*+) = (+)

(*-) :: BFPtr -> BFPtr -> BFPtr
(*-) = (-)

ptrSubtract :: BFPtr -> BFPtr -> BFPtr
ptrSubtract = subtract

ptrBounds :: (BFPtr, BFPtr)
ptrBounds = (0, ptrMax - 1)

