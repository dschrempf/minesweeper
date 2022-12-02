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
frame xs = hLine : map bracket xs <> [hLine]
  where
    n = length $ head xs
    hLine = replicate (n + 2) '-'
    bracket ys = '|' : (ys ++ ['|'])

instance Show Board where
  show = unlines . frame . map (unwords . map show) . getBoard

type HasMine = Bool

type Solution = [[HasMine]]
