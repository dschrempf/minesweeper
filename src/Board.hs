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
    Solution,
    Position,
    Move (..),
    move,
  )
where

import qualified Data.Matrix as M

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

newtype Board a = Board {getBoard :: M.Matrix a}

-- | Ensure that boards are square.
board :: [[a]] -> Board a
board [] = error "board: empty"
board xs
  | all (== n) ls = Board $ M.fromLists xs
  | otherwise = error "board: not square"
  where
    n = length xs
    ls = map length xs

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

instance ToChar a => Show (Board a) where
  show = unlines . frame . map showLine . M.toLists . getBoard

type HasMine = Bool

type Solution = Board HasMine

type Position = (Int, Int)

data Move = Flag | Open

open :: Position -> Board Cell -> Either String (Board Cell)
open i (Board xs) = undefined

move :: Move -> Position -> Board Cell -> Either String (Board Cell)
move m p@(i, j) b@(Board xs)
  | i >= n = Left $ "move: x exceeds board size: " <> show i <> ">=" <> show n
  | j >= n = Left $ "move: y exceeds board size: " <> show j <> ">=" <> show n
  | otherwise = case (m, e) of
      (Flag, Unknown) -> Right $ Board $ M.setElem Mine p xs
      (Flag, Mine) -> Right b
      (Flag, _) -> Left "move: can not flag open cell"
      (Open, Neighbors _) -> Right b
      (Open, _) -> open p b
  where
    n = M.nrows xs
    e = xs M.! p
