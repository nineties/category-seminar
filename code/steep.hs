steep [] = True
steep (a:as) = a > sum as && steep as

steep' = fst . foldr f c
    where
    c = (True, 0)
    f a (b,s) = (a > s && b, a + s)
