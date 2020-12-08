module Maths where

import Data.List
import Data.List.Split
import System.Random


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
fibs n | n < 0     = []
       | otherwise = map fib [1..n]

-- |Calculates the n:th fibonacci number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

{- Prime Generation -}
-- Generates a prime
getPrime :: IO Integer
getPrime = do
    seed <- newStdGen
    randomPrime $ fst $ randomR (1,100000) seed

-- Recursive helper func to generate a prime
randomPrime :: Integer -> IO Integer
randomPrime n | isPrime n = return n
              | otherwise = do
                  seed <- newStdGen
                  randomPrime $ fst $ randomR (1,100000) seed


-- Checks if an Integer is prime
isPrime :: Integer -> Bool
isPrime n = all (\e -> rem n e /= 0) [2..n-1]

comb :: Integer -> Integer -> Integer
comb = undefined

perm :: Integer -> Integer -> Integer
perm = undefined
