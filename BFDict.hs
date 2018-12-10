{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DictionaryApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
module BFDict ( runBFDict ) where

import Control.Monad            ( forM_ )
import Control.Monad.Loops      ( whileM_ )
import Control.Monad.Trans      ( MonadIO, liftIO )
import Control.Monad.State      ( MonadState(..), StateT, evalStateT, get, put )
import Data.Array.Base          ( unsafeRead, unsafeWrite )
import Data.Array.IO            ( IOUArray, newArray )
import Data.Bool                ( bool )
import Data.ByteString.Internal ( c2w, w2c )
import Data.IORef               ( IORef, newIORef, modifyIORef', readIORef )
import Data.Word                ( Word8 )
import System.IO    --            ( isEOF )

import BFPtr
import MonadBF
import DictUtils

type BFMem = (IOUArray BFPtr Word8, IORef BFPtr)

runBFDict :: String -> IO ()
runBFDict prog = do
    arr <- newArray bfBounds 0 :: IO (IOUArray BFPtr Word8)
    ref <- newIORef 0 :: IO (IORef BFPtr)
    let parse :: MonadBF m => m ()
        parse = parseBF prog
    parse (( bfImpIO (arr, ref) ))
    -- (parseBF prog :: MonadBF m => m ()) (( bfImpIO (arr, ref) ))

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
         modifyIORef' ref (+ n)
     , decPtr = \n -> do
         modifyIORef' ref (subtract n)
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

{-
type BFMem = IORef (IOUArray BFPtr Word8, BFPtr)
bfImpIO :: BFMem -> MonadBF.Dict IO
bfImpIO ref = MonadBF.Dict
    { parent1 = getDict @(Monad IO)
    , incByte = \n -> do
         (arr,ptr) <- readIORef ref
         readArray arr ptr >>= writeArray arr ptr . (+) n
     , decByte = \n -> do
         (arr,ptr) <- readIORef ref
         readArray arr ptr >>= writeArray arr ptr . subtract n
     , incPtr = \n -> do
         modifyIORef ref $ second (+ n)
     , decPtr = \n -> do
         modifyIORef ref $ second (subtract n)
     , showByte = do
         (arr,ptr) <- readIORef ref
         readArray arr ptr >>= putChar . w2c
     , readByte = do
         (arr,ptr) <- readIORef ref
         byte <- isEOF >>= bool (c2w <$> getChar) (return 0)
         writeArray arr ptr byte
     , getByte = do
         (arr,ptr) <- readIORef ref
         readArray arr ptr
     }


test :: MonadBF m => m ()
test = incByte 97 >> incPtr 10 >> incByte 10 >> decPtr 10 >> showByte

testInIO :: IO ()
testInIO = do
    arr <- newArray bfBounds 0
    r <- newIORef 0 :: IO (IORef BFPtr)
    test (( bfImpIO (arr,r) ))
-}
