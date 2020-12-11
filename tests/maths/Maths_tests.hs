import Maths
import Test.QuickCheck

prop_fibs :: Int -> Bool
prop_fibs n | n > 0     = (length . fibs) n == n
            | otherwise = (length . fibs) n == 0
