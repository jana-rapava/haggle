{-# LANGUAGE Rank2Types #-}
module FunctionStore where
import Lloyd15

data FunctionStore a =
    FS {
        stopSuccess :: Matrix a -> Bool,
        stopFail :: [Matrix a] -> Bool,
--        pick :: [Matrix a] -> (Matrix a, [Matrix a]),
        rank :: Matrix a -> Integer -> Integer,
        prune :: (Eq a) => [Matrix a] -> [Matrix a] -> [Matrix a]
        }
