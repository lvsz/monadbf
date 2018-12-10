{-# LANGUAGE BangPatterns #-}
module MonadBF ( MonadBF(..), parseBF ) where

import Control.Arrow       ( first )
import Control.Monad.Loops ( whileM_ )
import Data.Word           ( Word8 )

import BFPtr

class Monad m => MonadBF m where
    incByte :: Word8 -> m ()
    decByte :: Word8 -> m ()
    incPtr :: BFPtr -> m ()
    decPtr :: BFPtr -> m ()
    showByte :: m ()
    readByte :: m ()
    getByte :: m Word8

parseBF :: MonadBF m => String -> m ()
parseBF [] = return ()
parseBF !(x:xs) = case x of
    '+' -> incByte n8 >> parseBF rest
    '-' -> decByte n8 >> parseBF rest
    '>' -> incPtr ptr >> parseBF rest
    '<' -> decPtr ptr >> parseBF rest
    '.' -> showByte >> parseBF xs
    ',' -> readByte >> parseBF xs
    '[' -> let (loop, rest') = extractLoop xs
           in whileM_ ((/= 0) <$> getByte) (parseBF loop) >> parseBF rest'
    ']' -> error "Bracket mismatch"
    _   -> parseBF rest
  where
    !(n, rest) = first (succ . length) $! span (== x) xs
    !ptr = BFPtr n
    !n8 = fromIntegral n

extractLoop :: String -> (String, String)
extractLoop = go (0 :: Int) . flip (,) []
  where
    go 0 (']':xs,ys) = (reverse ys, xs)
    go n (']':xs,ys) = go (n - 1) (xs,']':ys)
    go n ('[':xs,ys) = go (n + 1) (xs,'[':ys)
    go n (x:xs,ys)   = go n (xs, x:ys)
    go _ ([],_)      = error "Bracket mismatch"

