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

nFromInt :: Int -> N
nFromInt 0 = N0
nFromInt 1 = N1
nFromInt 2 = N2
nFromInt 3 = N3
nFromInt 4 = N4
nFromInt 5 = N5
nFromInt 6 = N6
nFromInt 7 = N7
nFromInt 8 = N8
nFromInt x = error $ "nFromInt: out of bounds: " <> show x

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

getSize :: Board -> Int
getSize (Board ss _) = M.nrows ss

inBound :: Int -> Position -> Bool
inBound n (x, y) = inBound1D x && inBound1D y
  where
    inBound1D z = z >= 1 && z <= n

-- | Create a board of given size, and with given mines (1 indexed).
--
-- Ensure that boards are square.
board :: Int -> [(Int, Int)] -> Board
board n xs
  | all (inBound n) xs = Board (M.matrix n n g) (M.matrix n n $ const Unknown)
  | otherwise = error "board: mine indices out of bounds"
  where
    g x = x `elem` xs

-- Assume list of string is a rectangle.
frame :: [String] -> [String]
frame [] = error "frame: empty board"
frame xs = is : hLineTop : zipWith bracket [1 ..] xs ++ [hLineBottom]
  where
    m = length xs
    n = length $ head xs
    hLineTop = "  ┌" ++ replicate (n + 2) '─' ++ "┐"
    hLineBottom = "  └" ++ replicate (n + 2) '─' ++ "┘"
    is = "    " ++ showLine [1 .. m]
    bracket :: Int -> String -> String
    bracket i ys = toChar i : " │ " ++ ys ++ " │"

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

isBomb :: Board -> Position -> Bool
isBomb (Board ss _) p = ss M.! p

nNeighbors :: Position -> Board -> N
nNeighbors (i, j) b = nFromInt $ sum $ map (toInt . isBomb b) neighbors
  where
    n = getSize b
    neighbors =
      filter
        (inBound n)
        [ (pred i, j),
          (pred i, pred j),
          (i, pred j),
          (succ i, pred j),
          (succ i, j),
          (succ i, succ j),
          (i, succ j),
          (pred i, succ j)
        ]
    toInt True = 1
    toInt False = 0

open :: Position -> Board -> Either String Board
open i (Board ss xs)
  | ss M.! i = Left "open: mine"
  | otherwise = Right undefined

move :: Move -> Position -> Board -> Either String Board
move m p@(i, j) b@(Board ss xs)
  | i < 1 = Left $ "move: i zero or negative: " <> show i
  | j < 1 = Left $ "move: j zero or negative: " <> show i
  | i > n = Left $ "move: i exceeds board size: " <> show i <> ">=" <> show n
  | j > n = Left $ "move: j exceeds board size: " <> show j <> ">=" <> show n
  | otherwise = case (m, e) of
      (Flag, Unknown) -> Right $ Board ss $ M.setElem Mine p xs
      (Flag, Mine) -> Right b
      (Flag, _) -> Left "move: can not flag open cell"
      (Open, Neighbors _) -> Right b
      (Open, _) -> open p b
  where
    n = M.nrows xs
    e = xs M.! p
