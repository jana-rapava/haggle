module BeFS where

import Path
import Expandable
import Control.Monad.State.Lazy
import Debug.Trace

befs :: (Expandable a) =>
        (a -> Int) ->
        State (SList (Path a), Int) [[a]]
befs f = do
        backlog0 <- fst . get
        case (getSList backlog0) of
                [] -> return []
                ((P (path0,rank0)):backlog1) -> let active = head path0,
                                                    result = prune (expand active) path0 in do
                                                        case (result of)
                                                            Fail -> do
                                                                    put (SList backlog1, 0)
                                                                    paths1 <- befs f
                                                                    return paths1
                                                            Success _ -> do
                                                                        put (SList backlog1, 0)
                                                                        paths1 <- befs f
                                                                        return (path0:paths1)
                                                            Sons sons -> let lists0 = lengthen path0 sons,
                                                                            paths0 = zip lists0 (map (rank f) lists0),
                                                                            backlog2 = addSorted backlog1 (P paths0) in do
                                                                                put (SList backlog2, 0)
                                                                                befs f

testBefs :: (Eq a, Show a, Expandable a) => a -> (a -> Int) -> [[Matrix a]]
testBefs b f = fst $ runState (befs f) (SList [b_path])
        where b_path = ([b], rank f [b])
