{-# LANGUAGE FlexibleInstances #-}

module Success2x2 where

import Lloyd15
import Expandable

success = "AB\
            \C."

successBoard = generateBoard '.' 2 2 success

instance Expandable (Matrix Char) where
        stopSuccess = (== successBoard)
        generateNbs = nextBoards
