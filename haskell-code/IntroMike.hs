module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

-- Tiere auf dem texanischen Highway

-- Gürteltier:
-- - tot oder lebendig
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

type Weight = Integer

data Animal =
    Dillo Liveness Weight
    deriving Show



