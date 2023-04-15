{-# LANGUAGE BangPatterns #-}
import System.Environment ( getArgs )
import System.IO          ( readFile )
import Criterion.Main

import BFPure
import BFState
import BFReaderT
import BFDict

main :: IO ()
main = do
    (file:_) <- getArgs
    !prog <- readFile file
    defaultMain . return $
        bgroup "bf" [ bench "State" $ whnfIO (runBFState prog)
                    , bench "ReaderT" $ whnfIO (runBFReaderT prog)
                    , bench "Dict" $ whnfIO (runBFDict prog)
                    , bench "Pure" $ whnf (runBFPure []) prog
                    ]

