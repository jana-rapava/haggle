module Lloyd15 where

import Control.Exception (assert)
import Data.Maybe (catMaybes, fromJust)
import Data.List (elemIndex)
--import Control.Monad.State.Lazy
--import Debug.Trace

-- data type for (sparse) matrix
data Matrix a = M {
                blank :: a,
                height :: Int,
                width :: Int,
                content :: [(Int, a)]
--                } deriving (Eq)
                } deriving (Eq, Show)

--show' [] = ""
--show' (a:b:c:d:ms) = show (map snd (a:b:c:[d])) ++  "\n" ++ show' ms
--show' x = show x
-- just for testing - incompatible with Read
--instance Show a => Show (Matrix a) where
--        show m = show' (content m)

data Swap a = S {
                posFrom :: Int,
                posTo :: Int
                } deriving (Eq, Show)

mkMatrix :: (a, Int, Int, [(Int,a)]) -> Matrix a
mkMatrix (b, h, w, d) = M {blank = b, height = h, width = w, content = d}

generateBoard :: a -> Int -> Int -> [a] -> Matrix a
generateBoard blank boardHeight boardWidth cellvals =
    let size = boardHeight * boardWidth in
        assert (length cellvals == size)
        mkMatrix $ (blank, boardHeight, boardWidth, zip [0..size-1] cellvals)

divisible :: Int -> Int -> Bool
divisible x m = x `mod` m == 0

bool2maybe :: Bool -> a -> Maybe a
bool2maybe p x = if p then Just x else Nothing

-- find adjacent positions to a given position based on board size
findAdjacent :: Int -> Int -> Int -> [Int]
findAdjacent boardHeight boardWidth pos =
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
    in catMaybes [pos %- boardWidth, pos %- 1, pos %+ 1, pos %+ boardWidth]

computeAdjacents :: Int -> Int -> [[Int]]
computeAdjacents boardHeight boardWidth = [findAdjacent boardHeight boardWidth pos | pos <- [0..size-1]]
    where size = boardHeight * boardWidth

findBlank :: (Eq a) => Matrix a -> Maybe Int
findBlank b = elemIndex (blank b) (map snd $ content b)

mkSwap :: (Int, Int) -> Swap a
mkSwap (x,y) = S {posFrom = x,  posTo = y}

generateSwaps :: (Eq a) => Matrix a  -> [Swap a]
generateSwaps b = map mkSwap $ zip (repeat blankPos) swapPoss
    where
        boardHeight = height b
        boardWidth = width b
        blankPos = fromJust (findBlank b)
        adjacent = computeAdjacents boardHeight boardWidth
        swapPoss = adjacent !! blankPos

applySwap :: Matrix a -> Swap a -> Matrix a
applySwap board s = mkMatrix (blank board, height board, width board,
                        l3 ++ (pos1, snd x):l4 ++ (pos2, snd y):l2)
    where
        b = content board
        pos1 = min (posFrom s) (posTo s)
        pos2 = max (posFrom s) (posTo s)
        (l1,x:l2) = splitAt pos2 b
        (l3,y:l4) = splitAt pos1 l1

nextBoards :: (Eq a) => Matrix a -> [Matrix a]
nextBoards b = map (applySwap b) (generateSwaps b)
