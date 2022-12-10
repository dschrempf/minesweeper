-- |
-- Module      :  Board
-- Description :  Types for boards
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  2 21:44:25 2022.
module Board
  ( N (..),
    Cell (..),
    Board,
    board,
    Position,
    Move (..),
    move,
  )
where

import qualified Data.Matrix as M

type HasMine = Bool

data N = N0 | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8

instance ToChar N where
  toChar N0 = '.'
  toChar N1 = '1'
  toChar N2 = '2'
  toChar N3 = '3'
  toChar N4 = '4'
  toChar N5 = '5'
  toChar N6 = '6'
  toChar N7 = '7'
  toChar N8 = '8'

data Cell = Unknown | Mine | Neighbors N

instance ToChar Cell where
  toChar Unknown = '?'
  toChar Mine = 'o'
  toChar (Neighbors n) = toChar n

data Board = Board
  { _solution :: M.Matrix HasMine,
    _getBoard :: M.Matrix Cell
  }

-- | Create a board of given size, and with given mines (0 indexed).
--
-- Ensure that boards are square.
board :: Int -> [(Int, Int)] -> Board
board n xs
  | all inBound xs = Board (M.matrix n n g) (M.matrix n n $ const Unknown)
  | otherwise = error "board: mine indices out of bounds"
  where
    inBound (x, y) = inBound1D x && inBound1D y
    inBound1D x = x >= 0 && x < n
    g x = x `elem` xs

-- Assume list of string is a rectangle.
frame :: [String] -> [String]
frame [] = error "frame: empty board"
frame xs = is : hLineTop : zipWith bracket [0 ..] xs ++ [hLineBottom]
  where
    m = length xs
    n = length $ head xs
    hLineTop = "  ┌" ++ replicate (n + 2) '─' ++ "┐"
    hLineBottom = "  └" ++ replicate (n + 2) '─' ++ "┘"
    is = "    " ++ showLine [0 .. m - 1]
    bracket :: Int -> String -> String
    bracket i ys = show i ++ " │ " ++ ys ++ " │"

class ToChar a where
  toChar :: a -> Char

instance ToChar Int where
  toChar i = head $ show (i `mod` 10)

showLine :: ToChar a => [a] -> String
showLine xs = unwords [[toChar x] | x <- xs]

instance Show Board where
  show = unlines . frame . map showLine . M.toLists . _getBoard

type Position = (Int, Int)

data Move = Flag | Open

open :: Position -> Board -> Either String Board
open i (Board ss xs) = undefined

move :: Move -> Position -> Board -> Either String Board
move m p@(i, j) b@(Board ss xs)
  | i >= n = Left $ "move: x exceeds board size: " <> show i <> ">=" <> show n
  | j >= n = Left $ "move: y exceeds board size: " <> show j <> ">=" <> show n
  | otherwise = case (m, e) of
      (Flag, Unknown) -> Right $ Board ss $ M.setElem Mine p xs
      (Flag, Mine) -> Right b
      (Flag, _) -> Left "move: can not flag open cell"
      (Open, Neighbors _) -> Right b
      (Open, _) -> open p b
  where
    n = M.nrows xs
    e = xs M.! p
