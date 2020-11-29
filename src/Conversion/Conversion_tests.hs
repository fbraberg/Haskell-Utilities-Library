import Conversion
import Test.QuickCheck


prop_binary :: Gen Bool
prop_binary = do
  x <- choose (1,10000) :: Gen Integer
  return $ x == fromBinary(toBinary x)
