module BFS where

import InfInt
import Expandable
import Path
import Haggle
import Control.Monad.State.Lazy
import Debug.Trace

maybeLengthen :: [[a]] -> [Result a] -> ([[a]], [[a]])
maybeLengthen [] [] = ([], [])
maybeLengthen (path:paths) (r:rs) = let (suc', backlog') = maybeLengthen paths rs in
                                        case (r) of
                                            Fail -> (suc', backlog')
                                            Success _ -> (path:suc', backlog')
                                            Sons sons -> (suc', (lengthen path sons) ++ backlog')

rankLength :: [a] -> Path a
rankLength xs = P (xs, length xs)

liftM2zip :: (a -> b -> c) -> [a] -> [b] -> [c]
liftM2zip f xs ys = map (uncurry f) (zip xs ys)

extend :: (Show a, Expandable a) => [[a]] -> ([Path a], [Path a])
extend ps = let
                actives = map head ps
                results' = map expand actives
                results = liftM2zip prune results' ps
                (suc', backlog') = maybeLengthen ps results
            in
                (map rankLength suc', map rankLength backlog')


bfs :: (Expandable a, Show a) =>
       InfInt ->
       State (Backlog a) [[a]]
--       State (SList (Path a), Int) [[a]]
bfs limit = do
              s <- get
              let backlog0 = getSList $ fst $ getBacklog $ s
                  depth = snd $ getBacklog $ s in
                      if (Fin depth >= limit || null backlog0)
                      then
                        return []
                      else
                        let
                          minRank = (snd . getPath . head) backlog0
                          (lvl, backlog1) = span ((== minRank) . snd . getPath) backlog0
                          (suc, backlog2) = extend (map (fst . getPath) lvl)
                          backlog3 = addSorted backlog2 backlog1
                        in do
                          put (B (SList backlog3, depth + 1))
                          paths <- bfs limit
                          return ((map (fst . getPath) suc) ++ paths)

testBfs :: (Eq a, Show a, Expandable a) => a -> InfInt -> [[a]]
testBfs b limit = fst $ runState (bfs limit) (B (SList [b_path], 0))
        where b_path = P ([b], 1)
