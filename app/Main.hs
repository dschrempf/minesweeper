-- |
-- Module      :  Main
-- Description :  Minesweeper
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  2 21:06:01 2022.
module Main
  ( main,
  )
where

import Board
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text.IO as T

readN :: IO Int
readN = do
  r <- parseOnly decimal <$> T.getLine
  case r of
    Left err -> putStrLn err >> readN
    Right n ->
      if n < 1
        then do
          putStrLn "Enter a positive number."
          readN
        else pure n

pMove :: Parser Move
pMove = (Flag <$ char 'f') <|> (Open <$ char 'o')

pPos :: Parser Position
pPos = do
  i <- decimal
  _ <- char ' '
  j <- decimal
  pure (i, j)

pMovePos :: Parser (Move, Position)
pMovePos = do
  m <- pMove
  _ <- char ' '
  p <- pPos
  pure (m, p)

play :: Board -> IO ()
play b = do
  print b
  putStrLn "Enter move (f|o i j)"
  r <- parseOnly pMovePos <$> T.getLine
  case r of
    Left err -> putStrLn err >> play b
    Right (m, p) -> case move m p b of
      Left err -> putStrLn err
      Right b' -> play b'

main :: IO ()
main = do
  putStrLn "Enter size of board."
  n <- readN
  putStrLn "Enter number of mines."
  m <- readN
  b <- randomBoard n m
  play b
