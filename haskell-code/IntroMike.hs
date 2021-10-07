module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

-- Tiere auf dem texanischen Highway

-- Gürteltier:
-- - tot oder lebendig
-- - Gewicht

-- discriminated union / enumeration / Aufzählung
data Liveness = Dead | Alive
  deriving Show

type Weight = Integer

data Animal =
    Dillo Liveness Weight
    deriving Show

dillo1 :: Animal
-- Gürteltier, lebendig, 10kg
dillo1 = Dillo Alive 10

dillo2 :: Animal
-- totes Gürteltier, 12kg
dillo2 = Dillo Dead 12






