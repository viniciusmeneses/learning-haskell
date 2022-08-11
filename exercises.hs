-- Course: https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3

-- Class 3
-- double x = x + x
quadruple y = double (double y)
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- Class 4
-- otherLast xs = xs !! (length xs - 1)
otherLast xs = head (reverse xs)

-- otherInit xs = take (length xs - 1) xs
otherInit xs = reverse (tail (reverse xs))

-- Class 5
-- ['a', 'b', 'c'] :: [Char]
-- ('a', 'b', 'c') :: (Char, Char, Char)
-- [(False, '0'), (True, '1')] :: [(Bool, Char)]
-- ([False, True], ['0', '1']) :: ([Bool], [Char])
-- [tail, init, reverse] :: [[a] -> [a]]

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Class 6
