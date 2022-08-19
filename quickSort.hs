quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort ys ++ [x] ++ quickSort zs
  where
    ys = [a | a <- xs, a <= x]
    zs = [b | b <- xs, b > x]
