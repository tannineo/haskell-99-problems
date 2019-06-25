module Q01 where

myLast :: [a] -> a
myLast []     = error "empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

--------------------------------------------------------------------------------

myLast' = foldr1 (const id)

-- Prelude> const 1 2
-- 1
-- Prelude> (flip const) 1 2
-- 2
myLast'' = foldr1 (flip const)

myLast''' = head . reverse

myLast'''' = foldl1 (curry snd)

myLast''''' [] = error "No end for empty lists!"
myLast''''' x  = x !! (length x -1)

