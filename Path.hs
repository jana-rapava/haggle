{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Path where

newtype Path a = P { getPath :: ([a], Int) } deriving (Show, Eq)

instance Eq a => Ord (Path a) where
        p1 <= p2 =  (snd . getPath) p1 <= (snd . getPath) p2

addSorted :: (Eq a) => [Path a] -> [Path a] -> [Path a]
addSorted = foldr insert where
                insert x [] = [x]
                insert x (y:ys) = if (x < y) then (x:y:ys)
                                else y : (insert x ys)

