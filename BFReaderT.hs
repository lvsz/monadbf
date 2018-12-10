{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BFReaderT ( BFReaderT, runBFReaderT ) where

import Control.Monad.Reader     ( ReaderT, MonadIO, MonadReader, ask, asks, liftIO, runReaderT )
import Data.Array.Base          ( unsafeRead, unsafeWrite )
import Data.Array.IO            ( IOUArray, newArray )
import Data.Bool                ( bool )
import Data.ByteString.Internal ( c2w, w2c )
import Data.IORef               ( IORef, newIORef, readIORef, modifyIORef' )
import Data.Word                ( Word8 )
import System.IO                ( isEOF )

import BFPtr.ExplicitDictionary
import MonadBF

type BFMem = (IOUArray BFPtr Word8, IORef BFPtr)

newtype BFReaderT a = BFReaderT (ReaderT BFMem IO a)
    deriving (Functor, Applicative, Monad, MonadReader BFMem, MonadIO)

instance MonadBF BFReaderT where
    incByte n = do
        !(arr,ref) <- ask
        !ptr <- liftIO $ fromIntegral <$> readIORef ref
        !byte <- liftIO $ unsafeRead arr ptr
        liftIO $ unsafeWrite arr ptr (byte + n)
    decByte n = do
        !(arr,ref) <- ask
        !ptr <- liftIO $ fromIntegral <$> readIORef ref
        !byte <- liftIO $ unsafeRead arr ptr
        liftIO $ unsafeWrite arr ptr (byte - n)
    incPtr n = do
        !ref <- asks snd
        liftIO $ modifyIORef' ref (*+ n)
    decPtr n = do
        !ref <- asks snd
        liftIO $ modifyIORef' ref (ptrSubtract n)
    showByte = do
        !(arr,ref) <- ask
        !ptr <- liftIO $ fromIntegral <$> readIORef ref
        !char <- liftIO $ w2c <$> unsafeRead arr ptr
        liftIO $ putChar char
    readByte = do
        !byte <- liftIO $ isEOF >>= bool (c2w <$> getChar) (return 0)
        !(arr,ref) <- ask
        !ptr <- liftIO $ fromIntegral <$> readIORef ref
        liftIO $ unsafeWrite arr ptr byte
    getByte = do
        !(arr,ref) <- ask
        !ptr <- liftIO $ fromIntegral <$> readIORef ref
        liftIO $ unsafeRead arr ptr

runBFReaderT :: String -> IO ()
runBFReaderT s = env >>= runBFReaderT' (parseBF s)
  where
    env = (,) <$> newArray ptrBounds 0 <*> newIORef 0
    runBFReaderT' (BFReaderT bf) = runReaderT bf

