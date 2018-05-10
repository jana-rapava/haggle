{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haggle where

import Control.Monad.State
import Path
import Data.List (find)

newtype SList a = SList { getSList :: [a] } deriving (Functor, Show, Eq)

newtype Backlog a = B { getBacklog :: (SList (Path a), Int)}

-- inject value into a State monad
with :: a -> State (Backlog a) [[a]]
with x = let
           path =  SList [P ([x],0)]
         in do
           put (B (path, 0))
           return [[x]]

safeHead :: [[a]] -> Maybe [a]
safeHead = find (\x -> True)

-- runState wrapper for one solution
getOneSolution :: (Show a) => State (Backlog a) [[a]] -> IO ()
getOneSolution = print . safeHead . fst . ((flip runState) (B (SList [],0)))

-- runState wrapper for <count> solutions
getManySolutions :: (Show a) => Int -> State (Backlog a) [[a]] -> IO ()
getManySolutions count = print . (take count) . fst . ((flip runState) (B (SList [],0)))

-- runState wrapper for all solutions
getAllSolutions :: (Show a) => State (Backlog a) [[a]] -> IO ()
getAllSolutions = print . fst . ((flip runState) (B (SList [],0)))
