import Language.Haskell.TH (safe)
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
-- Conditional Expression
-- safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else reverse (init (reverse xs))

-- Guarded Equations
-- safetail :: [a] -> [a]
-- safetail xs | null xs = []
--             | otherwise = reverse (init (reverse xs))

-- Pattern Matching
safetail :: [a] -> [a]
safetail [] = []
safetail (x : xs) = xs

-- Conditional Expression
-- (||) :: Bool -> Bool -> Bool
-- x || y = if x then True else
--          if y then True else False

-- Guarded Equations
-- (||) :: Bool -> Bool -> Bool
-- x || y | x = True
--        | y = True
--        | otherwise = False

-- Pattern Matching
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

(&&) :: Bool -> Bool -> Bool
-- x && y = if x then if y then True else False else False
x && y = if x then y else False
