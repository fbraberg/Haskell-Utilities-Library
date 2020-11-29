module Conversion.Conversion where

-- | Converts 10-base Integer to binary.
toBinary :: Int -> Int
toBinary n = read (concat(map show(toBinaryList n)))
  

toBinaryList :: Int -> [Int]
toBinaryList 0 = [0]
toBinaryList n | n `mod` 2 == 1 = (toBinaryList (n `div` 2) ++ [1])
               | n `mod` 2 == 0 = (toBinaryList (n `div` 2) ++ [0])
           

-- | Converts from binary to 10-base Integer.
fromBinary :: Int -> Int
fromBinary n = fromBinaryList(ns)
  where ns = map (\x -> read [x] :: Int) (show n)

fromBinaryList :: [Int] -> Int
fromBinaryList xs = sum (zipWith (*) calcList (reverse xs))
  where
    twoList = map (\x -> if x > 0 then x+1 else x) xs -- | Converts 1s to 2s in the list.
    calcList = zipWith (^) (reverse twoList) [0,1..] -- | Calculates the value for each element.


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
