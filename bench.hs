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
        bgroup "brainfuck" [ bench "StateT" $ whnfIO (runBFState prog)
                           , bench "ReaderT" $ whnfIO (runBFReaderT prog)
                           , bench "MonadBF.Dict" $ whnfIO (runBFDict prog)
                           , bench "Pure" $ whnf (runBFPure []) prog
                           ]
    {-
    (mode:file:_) <- getArgs
    prog <- readFile file
    case mode of
        "State" -> runBFState prog
        "ReaderT" -> runBFReaderT prog
        "Dict" -> runBFDict prog
        "Pure" -> do
            input <- BS.unpack <$> BS.getContents
            BS.putStr $ (runBFPure prog input)

-}
