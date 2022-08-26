sum :: [Int] -> Int
sum = foldr (+) 0

and :: [Bool] -> Bool
and = foldr (&&) True

product :: [Int] -> Int
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

length :: [a] -> Int
length = foldr (\_ n -> 1 + n) 0

reverse :: [a] -> [a]
reverse = foldr (\x xs -> xs ++ [x]) []
