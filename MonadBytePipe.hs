{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MonadBytePipe where

import Control.Arrow
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Internal
import Data.Monoid
import Data.Word
import System.IO

class Monad m => MonadBytePipe m where
    getByte :: m Word8
    putByte :: Word8 -> m ()

instance MonadBytePipe IO where
    getByte = BS.unpack <$> BS.hGet stdin 1 >>= \case
        [x] -> return x
        [ ] -> return 0
    putByte = BS.hPut stdout . BS.pack . return

type IOPipe = forall a. StateT (a, (Handle, Handle)) IO
instance forall a. MonadBytePipe (StateT (a, (Handle, Handle)) IO) where
    getByte = do
        h <- fst . snd <$> get
        BS.unpack <$> liftIO (BS.hGet h 1) >>= \case
            [x] -> return x
            [ ] -> return 0
    putByte b = do
        h <- snd . snd <$> get
        liftIO . BS.hPut h $ BS.pack [b]
        liftIO $ hFlush h

type PurePipe = (ByteString, Builder)
newtype BytePipeT m a = BytePipeT { unBytePipeT :: StateT PurePipe m a }
    deriving (Functor, Applicative, Monad, MonadState PurePipe)

instance Monad m => MonadBytePipe (BytePipeT m) where
    getByte = BS.uncons . fst <$> get >>= \case
        Just (x, xs) -> modify (first $ const xs) >> return x
        Nothing      -> return 0
    putByte b = modify (second (<> word8 b))

runBytePipeT :: BytePipeT m a -> PurePipe -> m (a, PurePipe)
runBytePipeT m p = runStateT (unBytePipeT m) p

type BytePipe = BytePipeT Identity

runBytePipe :: BytePipe a -> PurePipe -> (a, PurePipe)
runBytePipe m = runIdentity . runBytePipeT m

purePipe :: PurePipe
purePipe = (BS.empty, mempty)

{-
newtype BytePipeT m a = BytePipeT { runBytePipeT :: PurePipe -> m (a, PurePipe) }

instance Functor m => Functor (BytePipeT m) where
    fmap f m = BytePipeT $ \s ->
        fmap (first f) $ runBytePipeT m s

instance Applicative m => Applicative (BytePipeT m) where
    pure a = BytePipeT $ \s -> return (a, s)
    BytePipeT mf <*> BytePipeT mx = BytePipeT $ \s -> do
        (f, s') <- mf s
        (x, s'') <- mx s'
        return (f x, s'')

pipe :: Monad m => (PurePipe -> (a, PurePipe)) -> BytePipeT m a
pipe f = BytePipeT (return . f)

instance Monad m => MonadBytePipe (BytePipeT m) where
    getByte = pipe $ \(i, o) -> case BS.uncons i of
        Just (x, y) -> (x, (y, o))
        Nothing     -> (0, (i, o))
    putByte b = pipe $ \(i, o) -> ((), (i, o <> word8 b))

fromInputToOutput :: Monad m => ByteString -> BytePipeT m a -> m Builder
fromInputToOutput s m = snd . snd <$> runBytePipeT m (s, mempty)

type PurePipe = State (ByteString, Builder)

instance MonadBytePipe PurePipe where
    getByte = first BS.uncons <$> get >>= \case
        (Just (x, xs), ys) -> put (xs, ys) >> return x
        (Nothing, ys)      -> return 0
    putByte x = modify' (second (<> word8 x))

-}

