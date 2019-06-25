module Q05 where

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]

-- above use lots of computation for it repeatedly reconses the result as it is accumulated.
--------------------------------------------------------------------------------

-- origin definition in Prelude
myReverseFold = foldl (flip (:)) []

-- computationally closer to Prelude
myReverse' :: [a] -> [a]
myReverse' list = reverse' list []
  where
    reverse' [] reversed     = reversed
    reverse' (x:xs) reversed = reverse' xs (x:reversed)

myReverse'' :: [a] -> [a]
myReverse'' xs = foldr (\x fId empty -> fId (x : empty)) id xs []

myReverse''' :: [a] -> [a]
myReverse''' = foldl (\a x -> x:a) []

