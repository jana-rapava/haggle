module LDS where

import Lloyd15
import FunctionStore
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Debug.Trace

comb :: Int -> [a] -> [[a]]
comb k []
        | k == 0 = [[]]
        | otherwise = []
comb k (x:xs)
        | k == 0 = [[]]
        | otherwise = (map (x:) (comb (k-1) xs)) ++ (comb k xs)

genPick :: Show a => (Matrix a -> Int -> Int) -> [Matrix a] -> (Matrix a, [Matrix a])
genPick rank xs = (head res, tail res)
        where
                l = length xs
                ns = [l,l-1..1]
                res = map snd $ sortBy (compare `on` fst) $ zip (zipWith rank xs ns) xs

-- while we have nonzero number of available discrepancies, run as DFS
-- if we reach 0, return Nothing
selectNodes :: (Eq a, Show a) =>
        ([Matrix a] -> [Matrix a] -> [Matrix a]) ->
        (Matrix a -> Bool) ->
        ([Matrix a] -> Bool) ->
        [[Matrix a]] ->
        [Matrix a] ->
        Int ->
        Maybe (Matrix a, [[Matrix a]], [Matrix a])

selectNodes _ _ _ _ _ 0 = Nothing

selectNodes _ _ _ [[]] _ _ = Nothing

selectNodes prune stopSuccess stopFail ((b:bs):bss) [] _ = undefined

selectNodes prune stopSuccess stopFail ((b:bs):bss) path@(p:ps) d =
   if stopSuccess b
   then
        Just (b, bs:bss, ps)
   else
        if stopFail next
        then
            selectNodes prune stopSuccess stopFail (bs:bss) ps (d-1)
        else
            Just (b, bs:bss, ps)
   where next = prune (nextBoards b) (b:path)

selectNodes prune stopSuccess stopFail ([]:bss) [] d =
        selectNodes prune stopSuccess stopFail bss [] d

selectNodes prune stopSuccess stopFail ([]:bss) path@(p:ps) d =
        selectNodes prune stopSuccess stopFail bss ps d

-- LDS: iteratively restart search, where you don't call pick in 1,2,..,d points
-- 3 urovne: iteracia cez 1,2,..,d
-- iteracia cez mozne uzly bez heuristiky
-- samotne prehladavanie
-- hladaj len jedno riesenie??
-- ako rozumne ukladat stav? co je pre nas relevantne, aby sme vedeli dobre pokracovat? dava zmysel povedat, ze lds musi byt v postupnosti prehladavani vzdy posledne?
lds' :: (Eq a, Show a) =>
        Matrix a ->
        Int ->
        StateT ([Matrix a],[[Matrix a]]) (Reader (FunctionStore a)) [[Matrix a]]
lds' b d = do
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
            case selectNodes prune stopSuccess stopFail backlog path' d of
                Nothing -> return [path']
                Just (next2, backlog2, path2) -> do
                    put (path2, backlog2)
--                    put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- lds' next2 d
--                    paths <- lds' (trace (" \n next2: " ++ show next2) next2) blank
                    return (path':paths)
        else
              if (stopFail next)
--            if (trace ("\nnext: " ++ show next) stopFail next)
            then
                case selectNodes prune stopSuccess stopFail backlog path' d of
                    Nothing -> return []
                    Just (next2, backlog2, path2) -> do
                        put (b:path2, backlog2)
                        lds' next2 d
            else
                let (nextBoard, bl) = pick next in do
                    put (path', bl:backlog)
--                    put (path', trace ("\n bl: " ++ show bl) (bl:backlog))
                    lds' nextBoard (d-1)

lds :: (Eq a, Show a) => Matrix a -> Int -> FunctionStore a -> [[Matrix a]]
lds b dis fs = concat $ [fst $ runReader (runStateT (lds' b d) ([],[])) fs | d <- [0..dis]]
