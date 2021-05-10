{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Fib where

import Test.Hspec
import Test.QuickCheck

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

badfib :: Integer -> Integer
badfib n0 = aux n0 0 1
  where
    aux 0 _ b = b
    aux n a b = aux (n - 1) b (a + b)

fastfib :: Integer -> Integer
fastfib n0 = go n0 0 1
  where
    go :: Integer -> Integer -> Integer -> Integer
    go 0 a _ = a
    go n a b = go (n-1) b (a+b)

type Fib = Integer -> Integer

prop_fib0 :: Fib -> Bool
prop_fib0 f = f 0 == 0

prop_fib1 :: Fib -> Bool
prop_fib1 f = f 1 == 1

prop_fibn :: Fib -> Integer -> Property
prop_fibn f n = n > 1 ==> f n == f (n-1) + f (n-2)

spec :: Spec
spec = do
  describe "Fibonacci" $ do
    it "fib 0 = 0" $
      property (prop_fib0 fastfib)
    it "fib 1 = 1" $
      property (prop_fib1 fastfib)
    it "fib n = fib (n-1) + fib (n-2)" $
      property (prop_fibn fastfib)
