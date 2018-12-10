import qualified Data.ByteString.Lazy as BS
import Control.Monad      ( forM_ )
import System.Environment ( getArgs )
import System.IO          ( readFile )

import BFPure
import BFState
import BFReaderT
import BFDict

main :: IO ()
main = do
    (mode:file:_) <- getArgs
    prog <- readFile file
    runBFState prog
    case mode of
        "State" -> runBFState prog
        "ReaderT" -> runBFReaderT prog
        "Dict" -> runBFDict prog
        "Pure" -> do
            input <- BS.unpack <$> BS.getContents
            BS.putStr $ (runBFPure input prog)

