{-# LANGUAGE DictionaryApplications #-}

-----------------------------------------------------------------------------
--
-- Maybe not the best use of explicit dictionary applications, but it is
-- very similar to the modSemigroup example given in the paper.
--
-----------------------------------------------------------------------------
--
module BFPtr.ExplicitDictionary (
      BFPtr
    , mkSemigroupOp
    , ptrBounds
    , ptrSubtract
    , (*+)
    , (*-)
    ) where

import Data.List.NonEmpty ( NonEmpty( (:|) ) )
import Data.Semigroup     ( Semigroup.Dict(..), (<>) )

type BFPtr = Int

ptrMax :: Int
ptrMax = 30000

ptrBounds :: (BFPtr, BFPtr)
ptrBounds = (0, ptrMax - 1)

ptrSemigroupDict :: BFPtr -> (BFPtr -> BFPtr -> BFPtr) -> Semigroup.Dict Int
ptrSemigroupDict n op = Semigroup.Dict
    (\x y -> (x `op` y) `mod` n)
    (\(a :| as) -> foldr (\x acc -> acc `op` x) a as `mod` n)
    (\x a -> fromIntegral x * (0 `op` a) `mod` n)

mkSemigroupOp :: BFPtr -> (BFPtr -> BFPtr -> BFPtr) -> (BFPtr -> BFPtr -> BFPtr)
mkSemigroupOp mod op = (<>) (( ptrSemigroupDict mod op ))

-- notation inspired by C pointers
(*+) :: BFPtr -> BFPtr -> BFPtr
(*+) = mkSemigroupOp ptrMax (+)

ptrSubtract :: BFPtr -> BFPtr -> BFPtr
ptrSubtract = (*+) . negate

(*-) :: BFPtr -> BFPtr -> BFPtr
(*-) = flip ptrSubtract

