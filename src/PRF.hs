module PRF where

{-Definition of the natural numbers.-}
data Nat = Zero | Succ Nat

{-recNat is the recursion operator that allows defining recursive functions on
natural numbers.-}
recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat a _ Zero = a
recNat a h (Succ n) = h n (recNat a h n)

{-addR uses recNat to recursively apply the successor function Succ to the
 second argument n times, starting from the first argument m.-}
addR :: Nat -> Nat -> Nat
addR m n = recNat n (\_ y -> Succ y) m

{-idR uses recNat to recursively apply the successor function Succ to the
second argument y times, starting from Zero.-}
idR :: Nat -> Nat
idR m = recNat Zero (\_ y -> Succ y) m

{-constR uses recNat to return Succ Zero regardless of the input.-}
constR :: Nat -> Nat
constR m = recNat (Succ Zero) (\_ _ -> Succ Zero) m

{-multR uses recNat to recursively add the second argument n to the
previous result y m times, starting from Zero.-}
multR :: Nat -> Nat -> Nat
multR m n = recNat Zero (\_ y -> addR n y) m

{- predR uses recNat to recursively return the previous result x without
 applying any operation.-}
predR :: Nat -> Nat
predR m = recNat Zero (\x _ -> x) m

{-Uses recNat to recursively subtract 1 from the previous result y n times, starting from
m.-}
subR :: Nat -> Nat -> Nat
subR m n = recNat m (\_ y -> predR y) n