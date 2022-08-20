merge :: Ord a => [a] -> [a] -> [a]
merge left [] = left
merge [] right = right
merge (n1 : left) (n2 : right) | n1 <= n2 = n1 : merge left (n2 : right)
                               | otherwise = n2 : merge (n1 : left) right

halve :: [a] -> ([a], [a])
halve list = splitAt middle list
  where middle = ceiling ((fromIntegral . length) list / 2)

sort :: Ord a => [a] -> [a]
sort [] = []
sort [n] = [n]
sort list = merge (sort left) (sort right)
  where (left, right) = halve list
