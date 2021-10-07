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

feedAnimal :: Weight -> (Animal -> Animal) -- currying, schönfinkeln
feedAnimal amount (Dillo Alive w) = Dillo Alive (w + amount)
feedAnimal amount (Dillo Dead w) = Dillo Dead w
feedAnimal amount (Parrot s w) = Parrot s (w + amount)

-- Assoziativität, "Klammern sind egal"
-- (a + b) + c = a + (b + c)
-- (a * b) * c = a * (b * c)

-- Gegeben eine Menge/Typ t und eine Operation:
-- op :: t -> t -> t (binäre Operation, Kombinator)
-- op (op a b) c = op a (op b c)
-- Halbgruppe / Semigroup

-- Typklasse (KEINE OO-KLASSE! eher INTERFACE)
class Semigroup t where 
    op :: t -> t -> t 

-- neutrales Element / Identity / zero
-- a + 0 = 0 + a = a
-- a * 1 = 1 * a = a
-- Halbgruppe + neutrales Element = Monoid

class Semigroup t => Monoid t where
    zero :: t 

highway :: [Animal]
highway = [dillo1, dillo2, parrot1, parrot2]  -- Liste

-- Financial Contracts

-- mit Domänenexpert:in sprechen: einfaches Beispiel
-- Zero-Bond / Zero-Coupon Bond:
-- "Ich bekomme 100€ am 24. Dezember 2021"

-- einfaches Beispiel in "atomare Teile" zerlegen
-- 1. "Währung"
-- 2. "mehrere"
-- 3. "später"
-- dabei: suchen nach Selbstbezügen

-- Currency Swap
-- Am 24.12.2021: bekomme 100€ UND bezahle 100GBP

data Currency = EUR | CHF | GBP 
  deriving Show

data Date = Date String
  deriving (Show, Ord, Eq)

{-
data Contract =
    ZeroCouponBond Double Currency Date
  | Future
  | Everest
  -- ...
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond 100 EUR (Date "2021-12-24")
-}

data Contract =
    One Currency -- "bekomme 1EUR jetzt", "bekomme 1CHF jetzt"
  | Multiple Double Contract -- "bekomme 100ER jetzt"
  | Later Date Contract
  | Change Contract -- dreht alle Zahlungsrichtungen um
  | And Contract Contract
  deriving Show

zeroCouponBond :: Double -> Currency -> Date -> Contract
zeroCouponBond amount currency date =
    Later date (Multiple amount (One currency))

-- zcb1 = Later (Date "2021-12-24") (Multiple 100 (One EUR))
zcb1 = zeroCouponBond 100 EUR (Date "2021-12-24")
zcb2 = Change (zeroCouponBond 100 GBP (Date "2021-12-24"))

cswap = And zcb1 zcb2

currencySwap date amount1 currency1 amount2 currency2 =
    And (zeroCouponBond amount1 date currency1)
        (Change (zeroCouponBond amount2 date currency2))