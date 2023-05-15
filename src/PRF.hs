module PRF where

data Nat = Zero | Succ Nat

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

--addition function
addR :: Nat -> Nat -> Nat
addR m n = recNat n (\_ y -> Succ y) m

--identity function
idR :: Nat -> Nat
idR m = recNat Zero (\_ y -> Succ y) m

--constant functions
constR :: Nat -> Nat
constR m = recNat (Succ Zero) (\_ y -> Succ Zero) m

--multiplication function
multR :: Nat -> Nat -> Nat
multR m n = recNat Zero (\_ y -> addR n y) m

--predecessor function
predR :: Nat -> Nat
predR m = recNat Zero (\x _ -> x) m

--truncated subtraction function
subR :: Nat -> Nat -> Nat
subR m n = recNat m (\_ y -> predR y) n








