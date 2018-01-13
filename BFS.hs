module BFS where

import InfInt
import Expandable
import Path
import Data.Function (on)
import Control.Monad.State.Lazy
import Debug.Trace

maybeLengthen :: [[a]] -> [Result a] -> ([a], [a])
maybeLengthen [] [] = ([], [])
maybeLengthen (path:paths) (r:rs) = let (suc', backlog') = maybeLengthen paths rs in
                                        case (r) of
                                            Fail -> (suc', backlog')
                                            Success _ -> (path:suc', backlog')
                                            Sons sons -> (suc', (lengthen path sons) ++ backlog')

extend :: (Expandable a) => [[a]] -> ([Path a], [Path a])
extend ps = let
                actives = map head ps,
                results = map expand actives,
                (suc', backlog') = maybeLenghten ps results,
                rankLength = P . (rank length) . getPath
            in
                (map rankLength suc', map rankLength backlog')

bfs :: (Expandable a) => Int -> State (SList (Path a), Int) [[a]]
bfs limit = do
            backlog0 <- getSList . fst . get
            depth <- snd . get
            if (depth >= limit || null backlog0)
            then return []
            else
                let
                    minRank = (snd . getPath . head) backlog0,
                    (lvl, backlog1) = span ((== minRank) `on` snd) backlog0,
                    (suc, backlog2) = extend (map (fst . getPath) lvl),
                    backlog3 = addSorted backlog2 backlog1
                in do
                    put (SList backlog3, depth + 1)
                    paths <- bfs limit
                    return (suc ++ paths)

testBfs :: (Eq a, Show a) => Matrix a -> InfInt -> [[Matrix a]]
testBfs b limit = fst $ runState (bfs limit) (SList [b_path])
        where b_path = ([b], 1)
