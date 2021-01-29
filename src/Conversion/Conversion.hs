module Conversion(toBinary, fromBinary, Bit, Bin) where

data Bit = I | O deriving (Eq, Show)
data Bin = Bin { bits :: [Bit] } deriving Eq


instance Show Bin where
  show = concatMap (show . b2i) . bits

test = Bin [I,O,I,O,I,O]

-- | Function to covert Bit into Integer
b2i :: Bit -> Integer
b2i I = 1
b2i O = 0

-- | Converts 10-base Integer to binary.
toBinary :: Integer -> Bin
toBinary n = Bin(toBinaryList n)



toBinaryList :: Integer -> [Bit]
toBinaryList 0 = []
toBinaryList n | n `mod` 2 == 1 = (toBinaryList (n `div` 2) ++ [I])
               | n `mod` 2 == 0 = (toBinaryList (n `div` 2) ++ [O])


-- | Converts from binary to 10-base Integer.
fromBinary :: Bin -> Integer
fromBinary (Bin xs) = sum (zipWith (*) calcList (reverse (map (b2i) xs)))
  where
    twoList = map (\x -> if x == I then (b2i x)+1  else b2i x) xs
    calcList = zipWith(^) (reverse twoList) [0,1..]


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
