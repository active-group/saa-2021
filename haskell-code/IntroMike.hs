module IntroMike where

import Prelude hiding (Functor, Monad, Monoid, Semigroup)

-- Tier auf dem texanischen Highway ist eins der folgenden:
-- - Gürteltier - ODER -
-- - Papagei

-- Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig - UND -
-- - Gewicht

-- discriminated union / enumeration / Aufzählung
data Liveness = Dead | Alive
  deriving Show

type Weight = Integer

-- Zustand des Tiers zu einem bestimmten Zeitpunkt
data Animal =
      Dillo Liveness Weight
    | Parrot String Weight
    deriving Show

dillo1 :: Animal
-- Gürteltier, lebendig, 10kg
dillo1 = Dillo Alive 10

dillo2 :: Animal
-- totes Gürteltier, 11kg
dillo2 = Dillo Dead 11

-- Begrüßungspapagei, 1kg
parrot1 :: Animal
parrot1 = Parrot "Hallo!" 1
parrot2 :: Animal
parrot2 = Parrot "Tschüß" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo l w) = Dillo Dead w
runOverAnimal (Parrot s w) = Parrot "" w 







