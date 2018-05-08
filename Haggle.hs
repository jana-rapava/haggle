{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Haggle where

import Control.Monad.State
import Path
import Data.List (find)

newtype SList a = SList { getSList :: [a] } deriving (Functor, Show, Eq)

newtype Backlog a = B { getBacklog :: (SList (Path a), Int)}

--with :: a -> State (SList (Path a), Int) [[a]]
with :: a -> State (Backlog a) [[a]]
with x = let
           path =  SList [P ([x],0)]
         in do
           put (B (path, 0))
           return [[x]]
 -- return
safeHead :: [[a]] -> Maybe [a]
safeHead = find (\x -> True)

getOneSolution :: (Show a) => State (Backlog a) [[a]] -> IO ()
--getOneSolution :: (Show a) => State (SList (Path a), Int) [[a]] -> IO ()
getOneSolution = print . safeHead . fst . ((flip runState) (B (SList [],0)))

getManySolutions :: (Show a) => Int -> State (Backlog a) [[a]] -> IO ()
--getManySolutions :: (Show a) => Int -> State (SList (Path a), Int) [[a]] -> IO ()
getManySolutions count = print . (take count) . fst . ((flip runState) (B (SList [],0)))

getAllSolutions :: (Show a) => State (Backlog a) [[a]] -> IO ()
--getAllSolutions :: (Show a) => State (SList (Path a), Int) [[a]] -> IO ()
getAllSolutions = print . fst . ((flip runState) (B (SList [],0)))

--instance Functor (Backlog a) where

--instance Applicative (Backlog a) where

--instance Monad (Backlog a) where

-- restricted Monads
