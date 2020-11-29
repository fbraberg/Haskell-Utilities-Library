module Conversion.Conversion where

-- | Converts 10-base Integer to binary.
toBinary :: Integer -> Integer
toBinary n = read (concat(map show(toBinarylist n)))
  

toBinarylist :: Integer -> [Integer]
toBinarylist 0 = [0]
toBinarylist n | n `mod` 2 == 1 = (toBinarylist (n `div` 2) ++ [1])
               | n `mod` 2 == 0 = (toBinarylist (n `div` 2) ++ [0])
           

-- | Converts from binary to 10-base Integer.
fromBinary :: Integer -> Integer
fromBinary = undefined

-- | Converts 10-base Integer to hexadecimal.
toHex :: Integer -> Integer
toHex = undefined

-- | Converts from hexadecimal to 10-base Integer.
fromHex :: Integer -> Integer
fromHex = undefined

-- | Converts 10-base Integer  to octadecimal.
toOcta :: Integer -> Integer
toOcta = undefined

-- | Converts octadecimal to 10-base Integer.
fromOcta :: Integer -> Integer
fromOcta = undefined
