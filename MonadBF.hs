module MonadBF ( MonadBF(..), parseBF ) where

import Control.Monad.Loops ( whileM_ )
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

data BFCode = IncByte !Int
            | DecByte !Int
            | DecPtr  !Int
            | IncPtr  !Int
            | GetByte
            | PutByte
            | Loop    ![BFCode]
            | Comment String

instance Show BFCode where
    show (IncByte n) = replicate n '+'
    show (DecByte n) = replicate n '-'
    show (DecPtr n) = replicate n '<'
    show (IncPtr n) = replicate n '>'
    show GetByte = ","
    show PutByte = "."
    show (Loop xs) = "[" ++ concatMap show xs ++ "]"
    show (Comment s) = s

isBFCommand :: BFCode -> Bool
isBFCommand (Comment _) = False
isBFCommand _           = True

removeBFComments :: [BFCode] -> [BFCode]
removeBFComments = filter isBFCommand

bfCode :: Parsec String () BFCode
bfCode =  IncByte . length <$> many1 (char '+')
      <|> DecByte . length <$> many1 (char '-')
      <|> DecPtr  . length <$> many1 (char '<')
      <|> IncPtr  . length <$> many1 (char '>')
      <|> GetByte <$  char ','
      <|> PutByte <$  char '.'
      <|> Loop    <$> between (char '[') (char ']') (many bfCode)
      <|> Comment <$> many1 (noneOf "+-[]<>.,")

evalBFCode :: MonadBF bf => BFCode -> bf ()
evalBFCode cmd = case cmd of
    IncByte n -> incByte (fromIntegral n)
    DecByte n -> decByte (fromIntegral n)
    DecPtr n  -> decPtr n
    IncPtr n  -> incPtr n
    GetByte   -> readByte
    PutByte   -> showByte
    Loop xs   -> let cmds = map evalBFCode $ removeBFComments xs
                 in whileM_ ((/= 0) <$> getByte) (foldl1 (>>) cmds)
    Comment _ -> return ()

parseBF :: MonadBF bf => String -> bf ()
parseBF s = case removeBFComments <$> parse (many bfCode) "BF" s of
    Right [] -> return ()
    Right bf -> foldl1 (>>) $ map evalBFCode bf
    Left err -> errorWithoutStackTrace $ "Parsing error:\n" ++ show err

