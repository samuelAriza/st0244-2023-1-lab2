module PRF where

data Nat = Zero | Succ Nat deriving (Show)

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

--addition function
addP :: Nat -> Nat -> Nat
addP Zero n = n
addP (Succ m) n = Succ (addP m n)

addR :: Nat -> Nat -> Nat
addR m n = recNat n (\_ y -> Succ y) n

--identity function
idP :: Nat -> Nat
idP Zero = Zero
idP (Succ m) = Succ (id m)

idR :: Nat -> Nat
idR m = recNat Zero (\_ y -> Succ y) m 

--constant functions
constP :: Nat -> Nat
constP Zero = Succ Zero
constP (Succ m) = (constP m)

constR :: Nat -> Nat
constR m = recNat (Succ Zero) (\_ y -> Succ Zero) m

--multiplication function
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ m) n = addP n (mult n m)

multR :: Nat -> Nat -> Nat
multR m n = recNat Zero (\_ y -> addP n y) m
