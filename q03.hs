module Q03 where

elementAt :: [a] -> Int -> a
elementAt [] _     = error "empty list"
elementAt (x:_) 1 = x
elementAt (x:xs) n = if n <= 0
                        then error "indexes start from 1"
                        else elementAt xs (n - 1)

--------------------------------------------------------------------------------

elementAt' :: [a] -> Int -> a
elementAt' list i    = list !! (i-1)

elementAt'' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = fst . last $ zip xs [1..n]

elementAt''' xs n = head $ foldr ($) xs
                         $ replicate (n - 1) tail
-- Negative indices not handled correctly:
-- Main> elementAt''' "haskell" (-1)
-- 'h'

elementAt'''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = last $ take n xs

elementAt''''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = head . reverse $ take n xs

elementAt'''''' xs n
  | length xs < n = error "Index out of bounds"
  | otherwise = head $ drop (n - 1) xs

