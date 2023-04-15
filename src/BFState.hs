{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BFState ( BFState, runBFState ) where

import Control.Arrow            ( second )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Control.Monad.State      ( MonadState, StateT, evalStateT, get, put )
import Data.Array.IO            ( IOUArray, newArray, readArray, writeArray )
import Data.Bool                ( bool )
import Data.ByteString.Internal ( c2w, w2c )
import Data.Word                ( Word8 )
import System.IO                ( isEOF )

import BFPtr.ExplicitDictionary
import MonadBF

type BFMem = (IOUArray BFPtr Word8, BFPtr)

newtype BFState a = BFState (StateT BFMem IO a)
    deriving (Functor, Applicative, Monad, MonadState BFMem, MonadIO)

instance MonadBF BFState where
    incByte n = get >>= (\m -> readBFMem m >>= writeBFMem m . (+) n)
    decByte n = get >>= (\m -> readBFMem m >>= writeBFMem m . subtract n)
    incPtr n = second (*+ n) <$> get >>= put
    decPtr n = second (ptrSubtract n) <$> get >>= put
    showByte = getByte >>= liftIO . putChar . w2c
    readByte = liftIO (isEOF >>= bool getChar (return '\0')) >>= \c ->
        get >>= \m -> writeBFMem m (c2w c)
    getByte = get >>= readBFMem

readBFMem :: BFMem -> BFState Word8
readBFMem = liftIO . uncurry readArray

writeBFMem :: BFMem -> Word8 -> BFState ()
writeBFMem = (liftIO .) . uncurry writeArray

runBFState :: String -> IO ()
runBFState prog = bfMem >>= evalBFState (parseBF prog)
  where
    bfMem = flip (,) 0 <$> newArray ptrBounds 0
    evalBFState (BFState bf) = evalStateT bf

