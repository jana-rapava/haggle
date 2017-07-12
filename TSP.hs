{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (liftM2)
import Data.Function (on)
import Data.List (find, elemIndex)
import Data.Maybe (fromJust)

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

newtype Ziplist a = Ziplist {getZiplist :: [a]} deriving (Functor, Show, Eq)

instance Applicative Ziplist where
        pure x = Ziplist $ [x]
        Ziplist fs <*> Ziplist xs = Ziplist $ zipWith ($) fs xs

-- TODO: verify laws
instance Monad Ziplist where
        (Ziplist xs) >>= f = head' $ map f xs
                where head' = Ziplist . map head . map getZiplist

mkVertex :: (a, [(a, Int)], Status) -> Vertex a
mkVertex (l, nbs, v) = V { label = l, neighbours = nbs, visited = v }

generateNeighbours' :: (Eq a) => [a] -> [[a]]
generateNeighbours' ls = getZiplist $ liftM2 filter (fmap (/=) zls) (replicate' (length ls) zls)
        where
                zls = Ziplist ls
                replicate' n  (Ziplist xs) = Ziplist (replicate n xs)

generateNeighbours :: (Eq a) => [a] -> [[Int]] -> [[(a, Int)]]
generateNeighbours ls dists =  liftM2 zip nbs dists
        where
            nbs = generateNeighbours' ls

generateGraph :: (Eq a) => [a] -> [[Int]] -> a -> Graph a
generateGraph ls dists start = map mkVertex $ zip3 ls (generateNeighbours ls dists) (mark start ls)
        where mark start ls = foldl (\acc x -> (if (x == start) then Expanded else Unvisited):acc) [] ls

findExpanded :: (Eq a) => Graph a -> Maybe Int
findExpanded g = elemIndex Expanded (map visited g)

getVertex :: (Eq a) => Graph a -> a -> (Vertex a, Int)
getVertex g l = (v, fromJust $ elemIndex v g)
        where v = fromJust $ find ((==l) . label) g

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

