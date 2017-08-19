{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TSP where

import Control.Monad (liftM2)
import Data.Function (on)
import Data.List (find, elemIndex)
import Data.Maybe (fromJust)
import Ziplist

data Status = Processed | Expanded | Unvisited
        deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Vertex a = V {
                label :: a,
                neighbours :: [(a, Int)],
                visited :: Status } deriving (Eq, Show)

data Move a = M {
                posFrom :: Int,
                posTo :: Int
                } deriving (Eq, Show)

type Graph a = [Vertex a]


mkVertex :: (a, [(a, Int)], Status) -> Vertex a
mkVertex (l, nbs, v) = V { label = l, neighbours = nbs, visited = v }

generateNeighbours' :: (Eq a) => [a] -> [[a]]
generateNeighbours' ls = map (\(p,s) -> init p ++ s) [splitAt l ls | l <- [1..(length ls)]]

generateNeighbours :: (Eq a) => [a] -> [[Int]] -> [[(a, Int)]]
generateNeighbours ls dists =  getZiplist $ liftM2 zip znbs zdists
        where
            zdists = foldr zcons (Ziplist []) dists
            znbs = Ziplist $ generateNeighbours' ls

generateGraph :: (Eq a) => [a] -> [[Int]] -> a -> Graph a
generateGraph ls dists start = map mkVertex $ zip3 ls (generateNeighbours ls dists) (map mark ls)
        where mark = \x -> if (x == start) then Expanded else Unvisited

findExpanded :: (Eq a) => Graph a -> Maybe Int
findExpanded g = elemIndex Expanded (map visited g)

getVertex :: (Eq a) => Graph a -> a -> (Vertex a, Int)
getVertex g l = (v, i)
    where
        v = fromJust $ find ((==l) . label) g
        i = fromJust $ elemIndex v g

getUnvisited :: (Eq a) => Graph a -> [a] -> [Int]
getUnvisited g ls = map snd $ filter ((==Unvisited) . visited . fst) $ map (getVertex g) ls

-- adjacent = neighbours which were not visited yet
-- what if they were expanded in a different branch?
computeAdjacent :: (Eq a) => Graph a -> Int -> [Int]
computeAdjacent g i = getUnvisited g (map fst $ neighbours (g !! i))

mkMove :: (Int, Int) -> Move a
mkMove (x,y) = M {posFrom = x,  posTo = y}

generateMoves :: (Eq a) => Graph a  -> [Move a]
generateMoves g = map mkMove $ zip (repeat e) moveTo
     where
         -- optimization: fusion ???
         e = fromJust (findExpanded g)
         moveTo = computeAdjacent g e

-- find start vertex, mark it as processed
-- find end vertex, mark it as expanded
applyMove :: (Eq a) => Graph a -> Move a -> Graph a
applyMove g m = l3 ++ (y1:l4) ++ (x1:l2)
     where
         pos1 = min (posFrom m) (posTo m)
         pos2 = max (posFrom m) (posTo m)
         (l1,x:l2) = splitAt pos2 g
         (l3,y:l4) = splitAt pos1 l1
         tmp = label (g !! (posFrom m))
         x1 = if (label x == tmp) then mkVertex (label x, neighbours x, Processed) else mkVertex (label x, neighbours x, Expanded)
         y1 = if (label y == tmp) then mkVertex (label y, neighbours y, Processed) else mkVertex (label y, neighbours y, Expanded)

nextNeighbours :: (Eq a) => Graph a -> [Graph a]
nextNeighbours g = map (applyMove g) (generateMoves g)
