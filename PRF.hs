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
addR m n = recNat n (\_ y -> Succ y) m

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
multP :: Nat -> Nat -> Nat
multP Zero _ = Zero
multP _ Zero = Zero
multP (Succ m) n = addR n (multP n m)

multR :: Nat -> Nat -> Nat
multR m n = recNat Zero (\_ y -> addR n y) m


--exponentiation function
expP :: Nat -> Nat -> Nat
expP Zero _ = Zero
expP _ Zero = Succ Zero
expP m (Succ n) = multR m (expP m n) 

expR :: Nat -> Nat -> Nat
expR m n = recNat (Succ Zero) (\_ y -> multR m y) n

--factorial function
factP :: Nat -> Nat
factP Zero = Succ Zero
factP (Succ m) = multR (Succ m) (factP m)










