{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Quicksort where

import Test.Hspec
import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort []     =  []
qsort (x:xs) =  qsort (filter (<= x) xs) ++
                [x] ++
                qsort (filter (> x) xs)

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [_] = True
isSorted (x:xs@(x':_))
  | x <= x'   = isSorted xs
  | otherwise = False

fakesort :: [a] -> [a]
fakesort _ = []

prop_fakesort1 :: [Int] -> Bool
prop_fakesort1 l = isSorted $ fakesort l

prop_qsort1 :: [Int] -> Bool
prop_qsort1 l = isSorted $ qsort l

prop_qsort2 :: [Int] -> Bool
prop_qsort2 l = qsort l == qsort (reverse l)

prop_qsort3 :: [Int] -> [Int] -> Bool
prop_qsort3 l1 l2 = qsort (l1 ++ l2) == qsort (l2 ++ l1)

prop_qsort4 :: [Int] -> Bool
prop_qsort4 l = length (qsort l) == length l

spec :: Spec
spec = do
  describe "Quicksort" $ do
    it "sort empty" $
      qsort [] `shouldBe` ([] :: [Int])
    it "sort sorted" $
      qsort [1,2,3] `shouldBe` ([1,2,3] :: [Int])
    it "sort unsorted" $
      qsort [3,2,1] `shouldBe` ([1,2,3] :: [Int])
    it "qsort1" $
      property prop_qsort1
    it "qsort2" $
      property prop_qsort2
    it "qsort3" $
      property prop_qsort3
