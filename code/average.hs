sum'    = foldr (\a x -> a + x) 0
length' = foldr (\a x -> x + 1) 0
average ls = sum' ls / length' ls
average' ls = s / l
    where
    (s, l) = foldr (\a (x, y) -> (a + x, y + 1)) (0, 0) ls



