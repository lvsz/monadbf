module MonadBF ( MonadBF(..), parseBF ) where

import Control.Monad.Loops ( whileM_ )
import Data.Functor        ( ($>) )
import Data.Word           ( Word8 )
import Text.ParserCombinators.Parsec

import BFPtr.ExplicitDictionary

class Monad m => MonadBF m where
    incByte  :: Word8 -> m ()
    decByte  :: Word8 -> m ()
    incPtr   :: BFPtr -> m ()
    decPtr   :: BFPtr -> m ()
    showByte :: m ()
    readByte :: m ()
    getByte  :: m Word8

parseBFCommand :: MonadBF bf => Parser (bf ())
parseBFCommand =  char '+' `is` incByte
              <|> char '-' `is` decByte
              <|> char '<' `is` decPtr
              <|> char '>' `is` incPtr
              <|> char ','  $>  readByte
              <|> char '.'  $>  showByte
              <|> whileM_ ((/= 0) <$> getByte) <$> loop
  where
    c `is` f = f . fromIntegral . length <$> many1 c
    loop = between (char '[') (char ']') $ foldl1 (>>) <$> many parseBFCommand

parseBF' :: MonadBF bf => String -> Either ParseError (bf ())
parseBF' = parse (foldl1 (>>) <$> many parseBFCommand) "bf" . filter (`elem` "+-<>,.[]")

parseBF :: MonadBF bf => String -> bf ()
parseBF s = case parseBF' s of
    Right bf -> bf
    Left err -> error $ "Parsing error:\n" ++ show err

