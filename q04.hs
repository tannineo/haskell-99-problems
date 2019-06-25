module Q04 where

myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs


--------------------------------------------------------------------------------

myLength' :: [a] -> Int
myLength' list = myLength_acc list 0
  where
    myLength_acc [] n     = n
    myLength_acc (_:xs) n = myLength_accxs (n + 1)

myLength1 =  foldl (\n _ -> n + 1) 0
myLength2 =  foldr (\_ n -> n + 1) 0
myLength3 =  foldr (\_ -> (+1)) 0
myLength4 =  foldr ((+) . (const 1)) 0
myLength5 =  foldr (const (+1)) 0
myLength6 =  foldl (const . (+1)) 0

myLengthZip xs = snd $ last $ zip xs [1..]
myLengthZip' = snd . last . (flip zip [1..])
myLengthZip'' = fst . last . zip [1..]

myLengthSum1 = sum . map (\_ -> 1)

