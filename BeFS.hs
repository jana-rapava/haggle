{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BeFS where

import Lloyd15
import FunctionStore
import Data.Function (on)
import Data.List (sortBy)
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Debug.Trace

type Path a = ([Matrix a], Int)

newtype SortedList a = SortedList {getSortedList :: [a]} deriving (Functor, Show, Eq)

split :: [Path a] -> Maybe (Path a, [Path a])
split [] = Nothing
split (p:ps) = Just (p, ps)

-- prolong path by all the possible next state, rank new paths
pickAndMerge :: Show a => (Matrix a -> Int -> Int) -> [Matrix a] -> Path a -> [Path a]
pickAndMerge rank bs (path,r) = zip (zipWith (:) (map fst sbs) (repeat path)) (map snd sbs)
        where
            l = length bs
            ns = [1..l]
            rbs = zip bs (zipWith rank bs ns)
            sbs = sortBy (compare `on` snd) rbs

insert :: Path a -> [Path a] -> [Path a]
insert (p,r) [] = [(p,r)]
insert (p1,r1) ((p2,r2):ps) = if (r1 > r2) then (p2,r2):(insert (p1,r1) ps) else (p1,r1):(p2,r2):ps

-- sort new paths into backlog
addTo :: [Path a] -> [Path a] -> [Path a]
addTo backlog ps = foldr insert backlog ps

befs' :: (Eq a, Show a) =>
        Path a ->
        StateT (SortedList (Path a)) (Reader (FunctionStore a)) [[Matrix a]]
befs' path = do
        stopSuccess <- asks stopSuccess
        stopFail <- asks stopFail
        rank <- asks rank
        prune <- asks prune
        backlog' <- get
        let
            b = (head . fst) path
            backlog = getSortedList backlog'
            next = prune (nextBoards b) (fst path) in
--          next = prune (nextBoards [b] blank) (trace ("\npath': " ++ show (map content path') ++ "\nbacklog: " ++ show (((map.map) content) backlog)) path') in
            if (stopSuccess b)
            then
                case split backlog of
                Nothing -> return [fst path]
                Just (path2, backlog2) -> do
                    put (SortedList backlog2)
--                  put ((trace ("\n path2: " ++ show path2) path2), (trace ("\nbacklog2: " ++ show backlog2) backlog2))
                    paths <- befs' path2
--                    paths <- dfs' (trace (" \n next2: " ++ show next2) next2) blank
                    return ((fst path):paths)
            else
                if (stopFail next)
--              if (trace ("\nnext: " ++ show next) stopFail next)
                then
                    case split backlog of
                    Nothing -> return []
                    Just (path2, backlog2) -> do
                        put (SortedList backlog2)
                        befs' path2
                else
                    let paths2 = pickAndMerge rank next path
                        backlog2 = addTo backlog paths2 in do
--                    put (path', trace ("\n bl: " ++ show bl ++ "\n backlog: " ++ show (backlog)) (bl:backlog))
                        put (SortedList $ tail backlog2)
                        befs' (head backlog2)

befs :: (Eq a, Show a) => Matrix a -> FunctionStore a -> [[Matrix a]]
befs b fs = fst $ runReader (runStateT (befs' b_path) (SortedList [([],0)])) fs
        where b_path = ([b],0)
