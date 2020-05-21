module Main where

import Data.List (sort)
import Test.QuickCheck

-----------------------------------------------------------
-- exercise #1

half x = x / 2
halfIdentity = (*2) . half

prop_halfIdentity :: Double -> Bool
prop_halfIdentity x = halfIdentity x == x

testHalf :: IO ()
testHalf = 
  quickCheck prop_halfIdentity

-----------------------------------------------------------
-- exercise #2

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y        (Nothing, t) = (Just y, t)
        go y        (Just x, t) = (Just y, x >= y)

intListGen = (arbitrary :: Gen [Int])
strListGen = (arbitrary :: Gen [String])

-- TODO refactor out commonality (generator is parameter)
prop_listOrderedInt :: Property
prop_listOrderedInt =
  forAll intListGen
  (\x -> listOrdered. sort $ x)

prop_listOrderedStr :: Property
prop_listOrderedStr =
  forAll strListGen
  (\x -> listOrdered. sort $ x)

testListOrdered :: IO ()
testListOrdered = do
  quickCheck prop_listOrderedInt
  quickCheck prop_listOrderedStr

-----------------------------------------------------------
-- exercise #3

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

prop_plusAssociative :: Property
prop_plusAssociative = 
  forAll (arbitrary :: Gen (Integer, Integer, Integer))
  (\(x, y, z) -> plusAssociative x y z)

prop_plusCommutative :: Property
prop_plusCommutative =
  forAll (arbitrary :: Gen (Integer, Integer))
  (\(x, y) -> plusCommutative x y)

testPlusAssocComm :: IO ()
testPlusAssocComm = do
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative

-----------------------------------------------------------
-- exercise #4 

multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x

prop_multAssociative :: Property
prop_multAssociative = 
  forAll (arbitrary :: Gen (Integer, Integer, Integer))
  (\(x, y, z) -> multAssociative x y z)

prop_multCommutative :: Property
prop_multCommutative =
  forAll (arbitrary :: Gen (Integer, Integer))
  (\(x, y) -> multCommutative x y)

testMultAssocComm :: IO ()
testMultAssocComm = do
  quickCheck prop_multAssociative
  quickCheck prop_multCommutative

-----------------------------------------------------------
-- exercise #5

nonZeroIntGen :: Gen Integer
nonZeroIntGen = (arbitrary :: Gen Integer) `suchThat` (/=0)

numDenomGen :: Gen (Integer, Integer)
numDenomGen = do
  num <- arbitrary
  denom <- nonZeroIntGen
  return (num, denom)

prop_quotRem :: Property
prop_quotRem =
  forAll numDenomGen
  (\(x, y) -> (quot x y) * y + (rem x y) == x)

prop_divMod :: Property
prop_divMod =
  forAll numDenomGen
  (\(x, y) -> (div x y) * y + (mod x y) == x)

testDivMod :: IO ()
testDivMod = do
  quickCheck prop_quotRem
  quickCheck prop_divMod

-----------------------------------------------------------
-- exercise #6

expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
expCommutative x y = x ^ y == y ^ x

natGen = (arbitrary :: Gen Integer) `suchThat` (>=0)

natPair :: Gen (Integer, Integer)
natPair = do
  x <- natGen
  y <- natGen
  return (x, y)

natTriple :: Gen (Integer, Integer, Integer)
natTriple = do
  x <- natGen
  y <- natGen
  z <- natGen
  return (x, y, z)

prop_expAssociative :: Property
prop_expAssociative = 
  forAll natTriple
  (\(x, y, z) -> expAssociative x y z)

prop_expCommutative :: Property
prop_expCommutative =
  forAll natPair
  (\(x, y) -> expCommutative x y)

testExpAssoc :: IO ()
testExpAssoc = quickCheck prop_expAssociative

testExpComm :: IO ()
testExpComm =  quickCheck prop_expCommutative

-----------------------------------------------------------
-- exercise #7

prop_dblReverse :: Property
prop_dblReverse =
  forAll (arbitrary :: Gen [String])
  (\x -> (reverse . reverse) x == id x)


test_dblReverse :: IO ()
test_dblReverse = quickCheck prop_dblReverse

-----------------------------------------------------------
-- exercise #8

-- prop_dollarSign :: Property
-- prop_dollarSign a =
--   f $ a == f a

-----------------------------------------------------------
-- exercise #9

prop_foldrColon :: Property
prop_foldrColon =
  forAll (arbitrary :: Gen ([Int], [Int]))
  (\(x, y) -> foldr (:) x y == (++) x y)

prop_foldrPlusPlus :: Property
prop_foldrPlusPlus =
  forAll (arbitrary :: Gen [[Int]])
  (\x -> foldr (++) [] x == concat x)

test_foldrColon :: IO ()
test_foldrColon = quickCheck prop_foldrColon

test_foldrPlusPlus :: IO ()
test_foldrPlusPlus = quickCheck prop_foldrPlusPlus

-----------------------------------------------------------
-- exercise #10

prop_takeLen :: Property
prop_takeLen =
  forAll (arbitrary :: Gen (Int, String))
  (\(n, xs) -> length (take n xs) == n)

-- *Main> prop_TL (NonNegative n) xs = length (take n xs) == n
-- *Main> quickCheck prop_TL

-----------------------------------------------------------
-- exercise #11

-- :{
-- prop_RS :: Int -> Bool
-- prop_RS x = (read (show x)) == x 
-- :}
-- quickCheck (verbose prop_RS)

-----------------------------------------------------------
main :: IO ()
main = do
  print "Run some tests, will ya!"
