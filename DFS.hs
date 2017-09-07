module DFS where

import Lloyd15
import FunctionStore
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Debug.Trace

-- return from recursion until we find a branching, manipulate path and backlog accordingly
selectPromising :: (Eq a, Show a) =>
        ([Matrix a] -> [Matrix a] -> [Matrix a]) ->
        (Matrix a -> Bool) ->
        ([Matrix a] -> Bool) ->
        [[Matrix a]] ->
        [Matrix a] ->
        Maybe (Matrix a, [[Matrix a]], [Matrix a])

selectPromising _ _ _ [[]] _ = Nothing

-- should never happen, but compiler would complain otherwise
selectPromising _ _ _ ((b:bs):bss) [] = undefined

-- we are in a child node, going down the same branch
selectPromising prune stopSuccess stopFail ((b:bs):bss) path@(p:ps) =
   if stopSuccess b
   then
        Just (b, bs:bss, ps)
   else
        if stopFail next
        then
            selectPromising prune stopSuccess stopFail (bs:bss) ps
        else
            Just (b, bs:bss, ps)
   where next = prune (nextBoards b) (b:path)

selectPromising prune stopSuccess stopFail ([]:bss) [] =
        selectPromising prune stopSuccess stopFail bss []

selectPromising prune stopSuccess stopFail ([]:bss) paths@(p:ps) =
        selectPromising prune stopSuccess stopFail bss ps

genPick :: Show a => (Matrix a -> Int -> Int) -> [Matrix a] -> (Matrix a, [Matrix a])
genPick rank xs = (head res, tail res)
        where
                l = length xs
                ns = [l,l-1..1]
                res = map snd $ sortBy (compare `on` fst) $ zip (zipWith rank xs ns) xs
dfs' :: (Eq a, Show a) =>
    Matrix a ->
    StateT ([Matrix a], [[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]

dfs' b = do
    stopSuccess <- asks stopSuccess
    stopFail <- asks stopFail
    rank <- asks rank
    prune <- asks prune
    (path, backlog) <- get
    let
        path' = b:path
        pick = genPick rank
        next = prune (nextBoards b) path' in
--        next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
        if (stopSuccess b)
        then
            case selectPromising prune stopSuccess stopFail backlog path' of
                Nothing -> return [path']
                Just (next2, backlog2, path2) -> do
                    put (path2, backlog2)
--                    put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- dfs' next2
--                    paths <- dfs' (trace (" \n next2: " ++ show next2) next2) blank
                    return (path':paths)
        else
              if (stopFail next)
--            if (trace ("\nnext: " ++ show next) stopFail next)
            then
                case selectPromising prune stopSuccess stopFail backlog path' of
                    Nothing -> return []
                    Just (next2, backlog2, path2) -> do
                        put (b:path2, backlog2)
                        dfs' next2
            else
                let (nextBoard, bl) = pick next in do
                    put (path', bl:backlog)
--                    put (path', trace ("\n bl: " ++ show bl ++ "\n backlog: " ++ show (backlog)) (bl:backlog))
                    dfs' nextBoard

dfs :: (Eq a, Show a) => Matrix a -> FunctionStore a -> [[Matrix a]]
dfs b fs = fst $ runReader (runStateT (dfs' b) ([],[])) fs
