import qualified Data.ByteString.Lazy as BS
import Control.Monad         ( forM_ )
import System.Console.GetOpt
import System.Environment    ( getArgs )
import System.Exit           ( ExitCode(..), exitWith )
import System.IO             ( hPutStr, readFile, stderr )

import BFPure
import BFState
import BFReaderT
import BFDict

data Mode = State
          | Reader
          | Dict
          | Pure
          deriving (Enum, Bounded, Show, Read)

modes :: [Mode]
modes = enumFromTo minBound maxBound

run :: Mode -> String -> IO ()
run mode = case mode of
    State  -> runBFState
    Reader -> runBFReaderT
    Dict   -> runBFDict
    Pure   -> \prog -> do
        input <- BS.unpack <$> BS.getContents
        BS.putStr $ runBFPure input prog

data Options = Options { optHelp       :: Bool
                       , optListModes  :: Bool
                       , optMode       :: Mode
                       , optTapeLength :: Int
                       }

defaultMode :: Mode
defaultMode = Dict

defaultTapeLength :: Int
defaultTapeLength = 30000

defaultOptions :: Options
defaultOptions = Options { optHelp       = False
                         , optListModes  = False
                         , optMode       = defaultMode
                         , optTapeLength = defaultTapeLength
                         }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['h'] ["help"]
          (NoArg (\opts -> opts { optHelp = True }))
          "Print this help message"
    , Option [] ["list-modes"]
          (NoArg (\opts -> opts { optListModes = True }))
          "List available Brainfuck modes."
    , Option ['m'] ["mode"]
          (ReqArg (\x opts -> opts { optMode = read x }) "<MODE>")
          ("Mode to use, default is " ++ show defaultMode)
    , Option ['n'] ["tape-length"]
          (ReqArg (\x opts -> opts { optTapeLength = read x }) "<NUM>")
          ("Number of bytes in a tape, default is " ++ show defaultTapeLength)
    ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv = case getOpt RequireOrder options argv of
    (opts, files, []) | optHelp opts'      -> putHelp
                      | optListModes opts' -> putModes
                      | otherwise          -> return (opts', files)
                      where opts' = foldl (flip ($)) defaultOptions opts
    (_, _, es) -> ioError $ userError $ concat es ++ usageInfo header options
  where
    header = "Usage: monadbf [OPTION...] [--] [files...]"
    putHelp = hPutStr stderr (usageInfo header options) >> exit
    putModes = putStr (unlines $ "Available modes:":map show modes) >> exit
    exit = exitWith ExitSuccess

main :: IO ()
main = do
    (opts, args) <- getArgs >>= parseOptions
    files <- mapM readFile args
    forM_ files $ run (optMode opts)

