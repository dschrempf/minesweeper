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
testBoardMin = board 1 []

testBoardMinSol :: String
testBoardMinSol =
  unlines
    [ "    1",
      "  \9484\9472\9472\9472\9488",
      "1 \9474 ? \9474",
      "  \9492\9472\9472\9472\9496"
    ]

testBoard :: Board
testBoard = board 3 []

testBoardSol :: String
testBoardSol = "    1 2 3\n  \9484\9472\9472\9472\9472\9472\9472\9472\9488\n1 \9474 ? ? ? \9474\n2 \9474 ? ? ? \9474\n3 \9474 ? ? ? \9474\n  \9492\9472\9472\9472\9472\9472\9472\9472\9496\n"

boardSpec :: Spec
boardSpec = describe "Board" $ do
  it "should have a correct show instance" $ do
    show testBoardMin `shouldBe` testBoardMinSol
    show testBoard `shouldBe` testBoardSol

main :: IO ()
main = hspec boardSpec
