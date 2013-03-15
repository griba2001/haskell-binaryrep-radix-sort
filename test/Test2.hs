module Test2 where

import Data.List.RadixSort.Base (msdSort, lsdSort, msdSortBy, lsdSortBy)
import Data.List as L
import Data.Int
import Data.Word
import Data.Ord
import Test.QuickCheck
import System.Random
import Distribution.TestSuite.QuickCheck2 (test)
import Distribution.TestSuite (Test)

data Rec = Rec {fieldA:: Float} deriving (Eq, Show)

instance Ord Rec where
  compare = comparing fieldA

instance Random Rec where
  randomR (Rec lo, Rec hi)  g = (Rec x, g')
    where
      (x, g') = randomR (lo, hi) g
      
  random g = (Rec x, g')
    where
      (x, g') = random g
            

instance Arbitrary Rec where
  arbitrary = choose (Rec lo, Rec hi)
    where
      (lo, hi) = (2^^(loExp-1), 2^^(hiExp-1))
      (loExp, hiExp) = floatRange (1::Float)
            

checkOrdered :: (Ord a) => [a] -> Bool    -- Eq a, Num a,
checkOrdered [] = True
checkOrdered [_] = True
checkOrdered list = L.all orderedPair $ L.zip list (L.tail list)
  where
    orderedPair = uncurry (<=)

-- deepCheck p = quickCheckWith (stdArgs { maxSuccess = 100, maxSize = 50}) p

tests :: [Test]
tests = [test "msd radix sort [Float]" ((\s -> checkOrdered $ msdSort s) :: [Float] -> Bool),
         test "msd radix sort [Double]" ((\s -> checkOrdered $ msdSort s) :: [Double] -> Bool),
         test "msd radix sort [Int64]" ((\s -> checkOrdered $ msdSort s) :: [Int64] -> Bool),
         test "msd radix sort [Int32]" ((\s -> checkOrdered $ msdSort s) :: [Int32] -> Bool),
         test "msd radix sort [Int16]" ((\s -> checkOrdered $ msdSort s) :: [Int16] -> Bool),
         test "msd radix sort [Int8]" ((\s -> checkOrdered $ msdSort s) :: [Int8] -> Bool),
         test "msd radix sort [Word64]" ((\s -> checkOrdered $ msdSort s) :: [Word64] -> Bool),
         test "msd radix sort [Word32]" ((\s -> checkOrdered $ msdSort s) :: [Word32] -> Bool),
         test "msd radix sort [Word16]" ((\s -> checkOrdered $ msdSort s) :: [Word16] -> Bool),
         test "msd radix sort [Word8]" ((\s -> checkOrdered $ msdSort s) :: [Word8] -> Bool),
         test "msd radix sort [Rec Float]" ((\s -> checkOrdered $ msdSortBy fieldA s) :: [Rec] -> Bool),
         
         test "lsd radix sort [Float]" ((\s -> checkOrdered $ lsdSort s) :: [Float] -> Bool),
         test "lsd radix sort [Double]" ((\s -> checkOrdered $ lsdSort s) :: [Double] -> Bool),
         test "lsd radix sort [Int64]" ((\s -> checkOrdered $ lsdSort s) :: [Int64] -> Bool),
         test "lsd radix sort [Int32]" ((\s -> checkOrdered $ lsdSort s) :: [Int32] -> Bool),
         test "lsd radix sort [Int16]" ((\s -> checkOrdered $ lsdSort s) :: [Int16] -> Bool),
         test "lsd radix sort [Int8]" ((\s -> checkOrdered $ lsdSort s) :: [Int8] -> Bool),
         test "lsd radix sort [Word64]" ((\s -> checkOrdered $ lsdSort s) :: [Word64] -> Bool),
         test "lsd radix sort [Word32]" ((\s -> checkOrdered $ lsdSort s) :: [Word32] -> Bool),
         test "lsd radix sort [Word16]" ((\s -> checkOrdered $ lsdSort s) :: [Word16] -> Bool),
         test "lsd radix sort [Word8]" ((\s -> checkOrdered $ lsdSort s) :: [Word8] -> Bool),
         test "lsd radix sort [Rec Float]" ((\s -> checkOrdered $ lsdSortBy fieldA s) :: [Rec] -> Bool)
        ]
        