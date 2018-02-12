module Expandable where

import Data.List ((\\))

data Result a = Fail | Success a | Sons [a]

class Eq a => Expandable a where
        stopSuccess :: a -> Bool
        stopFail :: [a] -> Bool
        rank :: (a -> Int) -> [a] -> Int
        prune :: Result a -> [a] -> Result a
        generateNbs :: a -> [a]

        stopFail = null
        rank f xs = f (head xs)

        prune Fail _ = Fail
        -- we checked when we added the node to the path
        -- !!! add check when putting start node into the monad
        prune (Success x) _ = Success x
        prune (Sons xs) ps = let xs' = xs \\ ps in
                                if (null xs') then Fail
                                else (Sons xs')

lengthen :: [a] -> [a] -> [[a]]
lengthen path sons = [s:path | s <- sons]

-- wrapper for simplification
expand :: (Expandable a) => a -> Result a
expand active = if (stopSuccess active) then Success active
                else let sons = generateNbs active in
                        if (stopFail sons) then Fail
                        else (Sons sons)
