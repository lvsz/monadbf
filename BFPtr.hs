{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BFPtr ( BFPtr(..), bfBounds ) where

import Data.Ix ( Ix )

bfPtrMax :: Integral a => a
bfPtrMax = 30000

newtype BFPtr = BFPtr Int deriving (Show, Eq, Ord, Ix, Integral, Real)
instance Num BFPtr where
    BFPtr a + BFPtr b = BFPtr ((a + b) `rem` bfPtrMax)
    BFPtr a * BFPtr b = BFPtr ((a * b) `rem` bfPtrMax)
    abs = id
    signum (BFPtr x) = BFPtr (signum x)
    fromInteger = BFPtr . fromInteger . flip rem bfPtrMax
    negate (BFPtr a) = BFPtr $ bfPtrMax - a
instance Bounded BFPtr where
    minBound = 0
    maxBound = pred bfPtrMax
instance Enum BFPtr where
    toEnum = BFPtr
    fromEnum (BFPtr x) = x
    succ = (+) 1
    pred = subtract 1

bfBounds :: (BFPtr, BFPtr)
bfBounds = (0, bfPtrMax - 1)

