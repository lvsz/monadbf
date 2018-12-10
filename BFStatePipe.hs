{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BFState ( BFState, runBF ) where

import Control.Arrow            ( first, second )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Control.Monad.State      ( MonadState, StateT, evalStateT, get, put )
import Data.Array.IO            ( IOUArray, newArray, readArray, writeArray )
import Data.Bool                ( bool )
import Data.ByteString.Internal ( c2w, w2c )
import Data.Word                ( Word8 )
import System.IO                ( Handle )

import BFPtr
import MonadBF
import MonadBytePipe

type BFMem = ((IOUArray BFPtr Word8, BFPtr), (Handle, Handle))

newtype BFState a = BFState { runBFState :: StateT BFMem IO a }
    deriving (Functor, Applicative, Monad, MonadState BFMem, MonadIO, MonadBytePipe)

instance MonadBF BFState where
    incByteM n = get >>= (\m -> readBFMem m >>= writeBFMem m . (+) n)
    decByteM n = get >>= (\m -> readBFMem m >>= writeBFMem m . subtract n)
    incPtrM n = first (second (+ n)) <$> get >>= put
    decPtrM n = first (second (subtract n)) <$> get >>= put
    showByteM = getByteM >>= liftIO . putByte
    readByteM = liftIO getByte >>= \c ->
        get >>= \m -> writeBFMem m c
    getByteM = get >>= readBFMem

readBFMem :: BFMem -> BFState Word8
readBFMem = liftIO . uncurry readArray . fst

writeBFMem :: BFMem -> Word8 -> BFState ()
writeBFMem = (liftIO .) . uncurry writeArray . fst

runBF :: String -> (Handle, Handle) -> IO ()
runBF prog pipe = bfMem >>= evalStateT (runBFState $ parseBF prog)
  where
    bfArr = flip (,) 0 <$> newArray bfBounds 0
    bfMem = flip (,) pipe <$> bfArr

