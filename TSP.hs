{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad (liftM2)

data Vertex a = V {
                label :: a,
                neighbours :: [a],
                visited :: Bool } deriving (Eq, Show)

type Graph a = [Vertex a]

newtype Ziplist a = Ziplist {getZiplist :: [a]} deriving (Functor, Show, Eq)

instance Applicative Ziplist where
        pure x = Ziplist $ [x]
        Ziplist fs <*> Ziplist xs = Ziplist $ zipWith ($) fs xs

-- TODO: verify laws
instance Monad Ziplist where
        (Ziplist xs) >>= f = head' $ map f xs
                where head' = Ziplist . map head . map getZiplist

mkVertex :: (a, [a], Bool) -> Vertex a
mkVertex (l, nbs, v) = V { label = l, neighbours = nbs, visited = v }

generateNeighbours :: (Eq a) => [a] -> [[a]]
generateNeighbours ls = getZiplist $ do
                        f <- fmap (/=) zls
                        xs <- (replicate' (length ls) zls)
                        return (filter f xs)
        where
                zls = Ziplist ls
                replicate' n  (Ziplist xs) = Ziplist (replicate n xs)

generateGraph :: (Eq a) => [a] -> Graph a
generateGraph ls = map mkVertex $ zip3 ls (generateNeighbours ls) (repeat False)

--nextNeighbours :: Graph a -> [Graph a]
