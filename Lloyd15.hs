{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lloyd15 where

import Control.Exception (assert)
import Data.Maybe (catMaybes, fromJust)
import Data.List (find)

type Matrix a = [(Int, a)]
data Swap a = S {
                posFrom :: Int,
                sym :: a,
                posTo :: Int
                } deriving (Eq, Show)

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

findIndex :: (a -> Bool) -> Matrix a -> Maybe Int
findIndex p b = do
                (pos, _) <- find (p . snd) b
                return pos

mkSwap :: (Int, a, Int) -> Swap a
mkSwap (x,y,z) = S {posFrom = x, sym = y, posTo = z}

generateSwaps :: (Eq a) => Int -> Int -> Matrix a -> (a -> Bool) -> [Swap a]
generateSwaps boardHeight boardWidth b blank = map mkSwap $ zip3
        (repeat blankPos) swapSyms swapPoss
                where
                   blankPos = fromJust (findIndex blank b)
                   adjacent = computeAdjacent boardHeight boardWidth
                   swapPoss = adjacent !! blankPos
                   swapSyms = map snd (map (b !!) swapPoss)
