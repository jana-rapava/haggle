{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lloyd15 where

import Control.Exception (assert)
import Data.Maybe (catMaybes)

adjacent = [[1,4],[0,2,5],[1,3,6],[2,7],
        [0,5,8], [1,4,6,9], [2,5,7,10], [3,6,11],
        [4,9,12], [5,8,10,13], [6,9,11,14], [7,10,15],
        [8,13], [9,12,14], [10,13,15], [11, 14]]

newtype Swap a = S ((Int,a), (Int,a)) deriving Show
type Matrix a = [(Int, a)]

generateBoard :: Int -> Int -> [a] -> Matrix a
generateBoard boardHeight boardWidth cellvals =
        let size = boardHeight * boardWidth in
                assert (length cellvals == size)
                zip [0..size-1] cellvals

divisible :: Int -> Int -> Bool
divisible x m = x `mod` m == 0

bool2maybe :: Bool -> a -> Maybe a
bool2maybe p x = if p then Just x else Nothing

grow :: Int -> Int -> Int -> [Int]
grow boardHeight boardWidth seed =
        let
                size = boardHeight * boardWidth
                x %- y = do
                        diff2 <- bool2maybe (diff >= 0) diff
                        bool2maybe (y /= 1 || (y == 1 && not (divisible x boardWidth))) diff2
                        where diff = x - y
                x %+ y = do
                                sum2 <- bool2maybe (sum < size) sum
                                bool2maybe (y /= 1 || (y == 1 && not (divisible sum boardWidth))) sum2
                        where sum = x + y


        in
                catMaybes [seed %- boardWidth, seed %- 1, seed %+ 1, seed %+ boardWidth]

computeAdjacent :: Int -> Int -> [[Int]]
computeAdjacent boardHeight boardWidth = [grow boardHeight boardWidth seed | seed <- [0..size-1]]
        where size = boardHeight * boardWidth
