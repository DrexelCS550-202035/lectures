{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Examples where

import Prelude hiding (length, map, take, drop, sum, foldr, foldl)

--
-- List examples
--

-- Compute the length of the list xs
length :: [a] -> Int
-- length xs = if null xs then 0 else 1 + length (tail xs)
length []     = 0
length (_:xs) = 1 + length xs

-- Compute the sum of a list of numbers
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

-- Return the nth element of a list, counting from 0.
nth :: Int -> [a] -> a
nth _ []     = error "nth: not enough elements"
nth 0 (x:_)  = x
nth n (_:xs) = nth (n-1) xs

-- Append two lists
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- Take the first n elements of a list
take :: Int -> [a] -> [a]
take 0 _      = []
take _ []     = []
take n (x:xs) = x : take (n-1) xs

-- Drop the first n elements of a list
drop :: Int -> [a] -> [a]
drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

--
-- Higher-order functions
--

-- Increment all elements of a list by 1
incAll :: Num a => [a] -> [a]
incAll []     = []
incAll (x:xs) = x+1 : incAll xs

-- Increment all elements of a list by a constant
addAll :: Num a => a -> [a] -> [a]
addAll _ []     = []
addAll n (x:xs) = x+n : addAll n xs

map :: (a -> b) -> ([a] -> [b])
map _ []     = []
map f (x:xs) = f x : map f xs

-- Calculate the squares of a list of numbers. Make the function non-recursive.
squares :: Num a => [a] -> [a]
squares = map (\x -> x*x)

curry :: ((a, b) -> c) -> a -> b -> c
-- curry f = \x -> \y -> f (x, y)
-- curry f = \x y -> f (x, y)
-- curry = \f x y -> f (x, y)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

(.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
(.) f g x = f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

-- What about papply?

add3 :: Int -> (Int -> (Int -> Int))
add3 x y z = x + y + z

--
-- Folds
--

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []     = z
--foldr f z (x:xs) = f x (foldr f z xs)
foldr f z (x:xs) = x `f` foldr f z xs

foldl :: (a -> b -> b) -> b -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f x z) xs
