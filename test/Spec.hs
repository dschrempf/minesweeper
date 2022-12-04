-- |
-- Module      :  Spec
-- Description :  Tests for Spec
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Dec  2 21:41:30 2022.
module Main
  ( main,
  )
where

import Board
import Test.Hspec

testBoardMin :: Board
testBoardMin = board [[Unknown]]

testBoard :: Board
testBoard =
  board
    [ [Unknown, Neighbors 1, Neighbors 0],
      [Neighbors 1, Neighbors 1, Neighbors 0],
      [Neighbors 0, Neighbors 0, Neighbors 0]
    ]

testBoardMinSol :: String
testBoardMinSol =
  unlines
    [ "    0",
      "  \9484\9472\9472\9472\9488",
      "0 \9474 ? \9474",
      "  \9492\9472\9472\9472\9496"
    ]

testBoardSol :: String
testBoardSol =
  unlines
    [ "    0 1 2",
      "  \9484\9472\9472\9472\9472\9472\9472\9472\9488",
      "0 \9474 ? 1 . \9474",
      "1 \9474 1 1 . \9474",
      "2 \9474 . . . \9474",
      "  \9492\9472\9472\9472\9472\9472\9472\9472\9496"
    ]

boardSpec :: Spec
boardSpec = describe "Board" $ do
  it "should have a correct show instance" $ do
    show testBoardMin `shouldBe` testBoardMinSol
    show testBoard `shouldBe` testBoardSol

main :: IO ()
main = hspec boardSpec
