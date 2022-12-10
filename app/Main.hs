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

readSize :: IO Int
readSize = do
  putStrLn "Enter size of board."
  r <- parseOnly decimal <$> T.getLine
  case r of
    Left err -> putStrLn err >> readSize
    Right n ->
      if n < 1
        then do
          putStrLn "Enter a positive number."
          readSize
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
  n <- readSize
  let b = board n []
  play b
