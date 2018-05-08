module BeFS where

import Path
import Expandable
import Backlog
import Control.Monad.State.Lazy
import Debug.Trace

befs :: (Expandable a, Show a) =>
        (a -> Int) ->
        State (Backlog a) [[a]]
--        State (SList (Path a), Int) [[a]]
befs f = do
           backlog0 <- get
           case (getSList $ fst $ getBacklog $ backlog0) of
                [] -> return []
                ((P (path0,rank0)):backlog1) -> let active = head (trace ("path0 " ++ show path0) path0)
                                                    result = prune (expand active) path0 in do
                                                        case (trace ("result " ++ show result) result) of
                                                            Fail -> do
                                                                    put (B (SList backlog1, 0))
                                                                    paths1 <- befs f
                                                                    return paths1
                                                            Success _ -> do
                                                                        put (B (SList backlog1, 0))
                                                                        paths1 <- befs f
                                                                        return (path0:paths1)
                                                            Sons sons -> let lists0 = lengthen path0 sons
                                                                             paths0 = zip lists0 (map (rank f) lists0)
                                                                             backlog2 = addSorted backlog1 (map P paths0) in do
                                                                                put (B (SList backlog2, 0))
                                                                                befs f

testBefs :: (Eq a, Show a, Expandable a) => a -> (a -> Int) -> [[a]]
testBefs b f = fst $ runState (befs f) (B (SList [b_path], 0))
        where b_path = P $([b], rank f [b])
