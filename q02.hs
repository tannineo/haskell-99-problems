module Q02 where

myButLast :: [a] -> a
myButLast []      = error "empty list"
myButLast (x:[])  = error "list with only one item"
myButLast (x:[y]) = x
myButLast (x:xs)  = myButLast xs

--------------------------------------------------------------------------------

myButLast' :: [a] -> a
myButLast' = last . init

myButLast'' x = reverse x !! 1

myButLast''' (x:(_:[])) = x
myButLast''' (_:xs)     = myButLast''' xs

myButLast'''' = head . tail . reverse

lastbut1 :: Foldable f => f a -> a
lastbut1 = fst . foldl (\(a,b) x -> (b,x)) (err1,err2)
  where
    err1 = error "lastbut1: Empty list"
    err2 = error "lastbut1: Singleton"

lastbut1safe :: Foldable f => f a -> Maybe a
lastbut1safe = fst . foldl (\(a,b) x -> (b,Just x)) (Nothing,Nothing)

myButLast''''' [] = error "Empty list"
myButLast''''' [x] = error "Too few elements"
myButLast''''' (x:xs) =
		if length xs == 1 then x
		else myButLast''''' xs

myButLast'''''' = head . reverse . init

