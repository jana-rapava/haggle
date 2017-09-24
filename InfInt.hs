module InfInt where

data InfInt = Fin Integer | Inf deriving (Eq, Show)

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

        fromInteger x = Fin x

        negate (Fin x) = Fin (negate x)
        negate Inf = Inf

