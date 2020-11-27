

module Maths.Maths where

import Data.List

-- |Calculates the median value of a list
median :: (Fractional a, Ord a) => [a] -> a
median lst | odd (length lst) = midItem1
           | otherwise        = midItem2
   where
       sortedLst = sort lst
       midItem1  = head $ take 1 $ drop (div (length sortedLst) 2) sortedLst
       midItem2  = mean $ take 2 $ drop (div (length sortedLst) 2-1) sortedLst

-- |Calculates the mean value of a list
mean :: Fractional a => [a] -> a
mean lst = sum lst / fromIntegral (length lst)

-- |Calculates the fibonacci sequence given a length
fibs :: Int -> [Int]
fibs n = map fib [1..n]

-- |Calculates the n:th fibonacci number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- |Indentity matrix of given size
idMatrix = undefined
