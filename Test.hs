import PRF
import Test.QuickCheck
import Numeric.Natural (Natural)

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink = shrinkIntegral

natToNatural :: Nat -> Natural
natToNatural Zero = 0
natToNatural (Succ n) = 1 + natToNatural n

naturalToNat :: Natural -> Nat
naturalToNat 0 = Zero
naturalToNat n = Succ (naturalToNat (n-1))

prop_idR :: Natural -> Bool
prop_idR n = natToNatural (idR (Succ n')) == natToNatural (Succ n')
  where
  n' :: Nat
  n' = naturalToNat n

main :: IO()
main = quickCheck prop_idR
