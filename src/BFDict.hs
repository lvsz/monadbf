{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DictionaryApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module BFDict ( runBFDict ) where

import Data.Array.Base          ( unsafeRead, unsafeWrite )
import Data.Array.IO            ( IOUArray, newArray )
import Data.Bool                ( bool )
import Data.ByteString.Internal ( c2w, w2c )
import Data.IORef               ( IORef, newIORef, modifyIORef', readIORef )
import Data.Word                ( Word8 )
import System.IO                ( isEOF, hFlush, stdout )

import BFPtr.ExplicitDictionary
import MonadBF
import DictUtils

type BFMem = (IOUArray BFPtr Word8, IORef BFPtr)

runBFDict :: String -> IO ()
runBFDict prog = do
    arr <- newArray ptrBounds 0 :: IO (IOUArray BFPtr Word8)
    ref <- newIORef 0 :: IO (IORef BFPtr)
    parseBF (( bfImpIO (arr, ref) )) prog

bfImpIO :: BFMem -> MonadBF.Dict IO
bfImpIO (arr,ref) = MonadBF.Dict
    { parent1 = getDict @(Monad IO)
    , incByte = \n -> do
         !ptr <- fromIntegral <$> readIORef ref
         x <- unsafeRead arr ptr
         unsafeWrite arr ptr (x + n)
     , decByte = \n -> do
         !ptr <- fromIntegral <$> readIORef ref
         unsafeRead arr ptr >>= unsafeWrite arr ptr . subtract n
     , incPtr = \n -> do
         modifyIORef' ref (*+ n)
     , decPtr = \n -> do
         modifyIORef' ref (ptrSubtract n)
     , showByte = do
         !ptr <- fromIntegral <$> readIORef ref
         unsafeRead arr ptr >>= putChar . w2c >> hFlush stdout
     , readByte = do
         !ptr <- fromIntegral <$> readIORef ref
         !byte <- isEOF >>= bool (c2w <$> getChar) (return 0)
         unsafeWrite arr ptr byte
     , getByte = do
         !ptr <- fromIntegral <$> readIORef ref
         unsafeRead arr ptr
     }

