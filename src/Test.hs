--Test module in which the tests are carried out
{-Instructing the GHC compiler not to
issue warnings related to orphan instances.-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-Import the file containing the definitions
of the five recursive primitive functions-}
import PRF

{-Import the Natural data type
from the Numeric.Natural module.-}
import Numeric.Natural ( Natural )

{-Import QuickCheck-}
import Test.QuickCheck
  ( Arbitrary ( arbitrary, shrink )
  , arbitrarySizedNatural
  , quickCheck
  , shrinkIntegral
  )

{-An instance of Arbitrary is
defined for the type Natural-}
instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

{-Function that converts Nat to Natural-}
natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

{-Function that converts Natural to Nat-}
naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n-1))

{-Function from naturals to booleans that compares the identity
function to return the same element it takes as an argument.-}
prop_idR :: Natural -> Bool
prop_idR n = natToNatural (idR (naturalToNat n))  == n

{-Function from naturals to booleans that compares that
the constant function, independent of the argument to
which the function is applied, always returns Succ Zero or 1.-}
prop_constR :: Natural -> Bool
prop_constR n =
  natToNatural (constR (naturalToNat n)) == natToNatural (Succ Zero)

{-Function that takes two naturals and returns a boolean.
Compares the return converted to Natural of the recursive
multiply function, which uses recNat, when applied to two random
arguments with the normal multiplication of those two arguments
using the infix operator (*).-}
prop_multR :: Natural -> Natural -> Bool
prop_multR m n =
  natToNatural (multR (naturalToNat m) (naturalToNat n)) == m * n

{-Function that takes a natural and returns a boolean.
Compare that the return converted to Natural
obtained by the application of the predecessor function,
is equal to the number m passed as argument of the function
minus one, in case the argument is greater than 0 and compares
that the return is equal to zero in case the argument
is something else.-}
prop_predR :: Natural -> Bool
prop_predR m | m > 0 = natToNatural (predR (naturalToNat m)) == m-1
             | otherwise = natToNatural (predR (naturalToNat m)) == 0

{-truncated subtraction function takes two arguments,
in case m is greater than or equal to n it applies the
function to m and n and compares that it is equal to
the subtraction with the infix operator (-), otherwise it
returns zero and compares it just with zero. -}
prop_subR :: Natural -> Natural -> Bool
prop_subR m n | m >= n =
  natToNatural (subR (naturalToNat m) (naturalToNat n)) == m-n
              | otherwise =
  natToNatural (subR (naturalToNat m) (naturalToNat n)) == 0

{-Main function-}
main :: IO()
main = do
  quickCheck prop_idR
  quickCheck prop_constR
  quickCheck prop_multR
  quickCheck prop_predR
  quickCheck prop_subR

