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
  ( Cell (..),
    Board,
    board,
    Solution,
  )
where

import Numeric.Natural

data Cell = Unknown | Mine | Neighbors Natural

instance Show Cell where
  show Unknown = "?"
  show Mine = "o"
  show (Neighbors 0) = "."
  show (Neighbors n) = show n

newtype Board = Board {getBoard :: [[Cell]]}

board :: [[Cell]] -> Board
board [] = error "board: empty"
board xs
  | all (== l) ls = Board xs
  | otherwise = error "board: not square"
  where
    l = length xs
    ls = map length xs

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

showLine :: Show a => [a] -> String
showLine = unwords . map show

instance Show Board where
  show = unlines . frame . map showLine . getBoard

type HasMine = Bool

type Solution = [[HasMine]]

move :: (Natural, Natural) -> Board -> Either String Board
move (x, y) = undefined
