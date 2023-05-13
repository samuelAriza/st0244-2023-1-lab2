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
prop_idR n = natToNatural (idR (naturalToNat n))  == n


prop_constR :: Natural -> Bool
prop_constR n = natToNatural (constR (naturalToNat n)) == natToNatural (Succ Zero)

prop_multR :: Natural -> Natural -> Bool
prop_multR m n = natToNatural (multR (naturalToNat m) (naturalToNat n)) == m * n



main :: IO()
main = do
  quickCheck prop_idR
  quickCheck prop_constR
  quickCheck prop_multR

