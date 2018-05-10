module InfInt where

data InfInt = Fin Int | Inf deriving (Eq, Show)

instance Num InfInt where
        (Fin x) + (Fin y) = Fin (x + y)
        Inf + _ = Inf
        _ + Inf = Inf

        (Fin x) * (Fin y) = Fin (x * y)
        Inf * _ = Inf
        _ * Inf = Inf

        abs (Fin x) = Fin (abs x)
        abs Inf = Inf

        signum (Fin x) = Fin (signum x)
        signum Inf = 1

        fromInteger x = Fin (fromInteger x)

        negate (Fin x) = Fin (negate x)
        negate Inf = Inf

instance Ord InfInt where
        (Fin x) `compare` (Fin y) = x `compare` y
        Inf `compare` (Fin x) = GT
        (Fin x) `compare` Inf = LT
        Inf `compare` Inf = EQ

