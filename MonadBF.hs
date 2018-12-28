module MonadBF ( MonadBF(..), parseBF ) where

import Control.Monad.Loops ( whileM_ )
import Data.Functor        ( ($>) )
import Data.Word           ( Word8 )
import Text.Parsec

import BFPtr.ExplicitDictionary

class Monad m => MonadBF m where
    incByte  :: Word8 -> m ()
    decByte  :: Word8 -> m ()
    incPtr   :: BFPtr -> m ()
    decPtr   :: BFPtr -> m ()
    showByte :: m ()
    readByte :: m ()
    getByte  :: m Word8

parseBFCommand :: MonadBF bf => Parsec String () (bf ())
parseBFCommand =  char '+' `is` incByte
              <|> char '-' `is` decByte
              <|> char '<' `is` decPtr
              <|> char '>' `is` incPtr
              <|> char ','  $>  readByte
              <|> char '.'  $>  showByte
              <|> whileM_ ((/= 0) <$> getByte) <$> loop
              <|> anyChar `parserBind` (unexpected . show)
  where
    c `is` f = f . fromIntegral . length <$> many1 c
    loop = between (char '[') (char ']') $ foldl1 (>>) <$> cmds
    cmds = let check [] = errorWithoutStackTrace "Error: non-terminating loop"
               check xs = xs
           in check <$> many parseBFCommand

isBFCommand :: Char -> Bool
isBFCommand = flip elem "+-<>,.[]"

parseBF :: MonadBF bf => String -> bf ()
parseBF s | null prog = return ()
          | otherwise = either parseError id bf
  where
    prog = filter isBFCommand s
    bf = parse (foldl1 (>>) <$> many parseBFCommand) "BF" prog
    parseError err = errorWithoutStackTrace $ "Parsing error:\n" ++ show err

