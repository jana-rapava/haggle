module Backlog where

import Control.Monad.State
import Path
import Data.List (find)

--newtype Backlog a = B { getBacklog :: State (SList (Path a), Int) [[a]]}

with :: a -> State (SList (Path a), Int) [[a]]
--with :: a -> Backlog a
with x = let
           path = SList [P ([x],0)]
         in do
           put (path, 0)
           return [[x]]
 -- return
safeHead :: [[a]] -> Maybe [a]
safeHead = find (\x -> True)

one :: (Show a) => State (SList (Path a), Int) [[a]] -> IO ()
one = print . safeHead . fst . ((flip runState) (SList [],0))

many :: (Show a) => Int -> State (SList (Path a), Int) [[a]] -> IO ()
many count = print . (take count) . fst . ((flip runState) (SList [],0))
--instance Functor (Backlog a) where

--instance Applicative (Backlog a) where

--instance Monad (Backlog a) where

-- restricted Monads
