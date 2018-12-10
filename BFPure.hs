{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
module BFPure ( BFPure, runBFPure ) where

import Control.Monad.State     ( MonadState, State, execState, get, put )
import Data.Array.Unboxed      ( UArray, (!), (//), accum, listArray )
import Data.ByteString.Builder ( Builder, toLazyByteString, word8 )
import Data.ByteString.Lazy    ( ByteString )
import Data.Monoid             ( (<>), mempty )
import Data.Word               ( Word8 )

import BFPtr
import MonadBF

data BFPureState = BFPureState { bfArray :: UArray BFPtr Word8
                               , bfPtr :: BFPtr
                               , input :: [Word8]
                               , output :: Builder
                               }

newtype BFPure a = BFPure (State BFPureState a)
    deriving (Functor, Applicative, Monad, MonadState BFPureState)

instance MonadBF BFPure where
    incByte n = get >>= \s@(BFPureState a i _ _) ->
        put $ s { bfArray = accum (+) a [(i, n)] }
    decByte n = get >>= \s@(BFPureState a i _ _) ->
        put $ s { bfArray = accum (-) a [(i, n)] }
    incPtr n = get >>= \s ->
        put $ s { bfPtr = bfPtr s + n }
    decPtr n = get >>= \s ->
        put $ s { bfPtr = bfPtr s - n }
    showByte = get >>= \s@(BFPureState a i _ o) ->
        put $ s { output = o <> word8 (a ! i) }
    readByte = get >>= \s@(BFPureState a i inp _) -> put $ case inp of
        (x:xs) -> s { bfArray = a // [(i, x)], input = xs }
        []     -> s { bfArray = a // [(i, 0)] }
    getByte = get >>= \(BFPureState a i _ _) -> return (a ! i)

runBFPure :: [Word8] -> String -> ByteString
runBFPure inp prog = execBFPure (parseBF prog) $! BFPureState arr 0 inp mempty
  where
    !arr = listArray bfBounds $ repeat 0
    execBFPure (BFPure bf) s = toLazyByteString . output $! execState bf s

