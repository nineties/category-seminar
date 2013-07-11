cons0 = foldr f c
    where
    c = [0]
    f a x = 0 : a : tail x
