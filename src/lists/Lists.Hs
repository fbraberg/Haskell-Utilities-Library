module Lists where

-- |Takes a list and returns the same list without duplicates
unique :: Eq a => [a] -> [a]
unique [] = []
unique (e:es) | not (elem e es) = e:(unique es)
              | otherwise         = unique es
