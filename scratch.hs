{-# LANGUAGE FlexibleInstances #-}

module Scratch where

import Path
import Expandable
import Backlog
import Lloyd15
import BFS
import BeFS
import TestFixtures
import Control.Monad.State


success = "ABCD\
           \EFGH\
           \IJKL\
           \MNO."

successBoard = generateBoard '.' 4 4 success

instance Expandable (Matrix Char) where
        stopSuccess = (== successBoard)
        generateNbs = nextBoards


f = do
          with board1y
          bfs 2
          befs manhattan_sum

-- safeHead = find (\x -> True)

-- one = print . safeHead . fst . ((flip runState) (SList [],0))

--many count = print . (take count) . fst . ((flip runState) (SList [],0))

main =  many 2 f
