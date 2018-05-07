{-# LANGUAGE FlexibleInstances #-}

module Success4x4 where

import Lloyd15
import Expandable

success = "ABCD\
           \EFGH\
           \IJKL\
           \MNO."

successBoard = generateBoard '.' 4 4 success

instance Expandable (Matrix Char) where
        stopSuccess = (== successBoard)
        generateNbs = nextBoards
