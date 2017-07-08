{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (liftM2)

data Status = Processed | Expanded | Unvisited
        deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Vertex a = V {
                label :: a,
                neighbours :: [(a, Int)],
                visited :: Status } deriving (Eq, Show)

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

--nextNeighbours :: Graph a -> [Graph a]
