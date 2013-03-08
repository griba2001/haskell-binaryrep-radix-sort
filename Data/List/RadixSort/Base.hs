{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE PackageImports, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-} -- , OverlappingInstances
{- | Radix sort (O(k n) where k= #digits) of lists of floats (based on its IEEE754 representation) or Int<N> or Word<N> based on their representation

  The lsd prefix is for Least significant digit radix sort, while msd is for the parallelized Most significant digit one.

  Here we partition numbers by sign and sort both lists in parallel (you should link with -threaded)

  The digit size is set to 8 bits.

  digit value queues are appended as difference lists (DList from package dlist, O(1) on append)

  The instance for 'Int' (machine word size) is not portable because it may have reserved bits for compiler use.
  The type Word may be restricted to the same number of bits as Int.
  Check The word size story at <http://www.haskell.org/ghc/docs/7.2.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html#g:1>

  A quickcheck test-suite has been added.
-}
-- @author: Gabriel Riba Faura
-- Internally uses (.$) = flip ($)

module Data.List.RadixSort.Base (
  msdSortInts, msdSortFloats, msdSortNats,
  lsdSortInts, lsdSortFloats, lsdSortNats,
  RadixRep(..)
) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.MSD (msdRadixSort)
import Data.List.RadixSort.Internal.LSD (lsdRadixSort)
import Data.List.RadixSort.Internal.Util (partBySign)

import qualified Data.List as L
import "parallel" Control.Parallel (par, pseq)

------------------------------------------
        
------------------------------------------

-- | sortFloats partitions between positive and negative and sort by binary repr. (exponent:mantissa) for each set in parallel,
-- reversing the negatives list after radixSort.
--
-- O(k n) where k= #digits, for each signed bag, plus sign partition, negatives reversing and reassembling
--
-- use this for Floats and Doubles
                      
msdSortFloats :: (RadixRep a) => [a] -> [a]
msdSortFloats [] = []
msdSortFloats [x] = [x]
msdSortFloats list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = msdRadixSort poss
    sortedNegs = negs .$ msdRadixSort
                      .$ L.reverse


lsdSortFloats :: (RadixRep a) => [a] -> [a]
lsdSortFloats [] = []
lsdSortFloats [x] = [x]
lsdSortFloats list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = lsdRadixSort poss
    sortedNegs = negs .$ lsdRadixSort
                      .$ L.reverse
                      
-- | sortInts partitions between positive and negative and sort each set in parallel
-- 
-- O(k n) where k= #digits, for each signed bag, plus sign partition and reassembling
-- 
-- use this for Int<N> types
msdSortInts :: (RadixRep a) => [a] -> [a]
msdSortInts [] = []
msdSortInts [x] = [x]
msdSortInts list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = msdRadixSort poss
    sortedNegs = msdRadixSort negs

lsdSortInts :: (RadixRep a) => [a] -> [a]
lsdSortInts [] = []
lsdSortInts [x] = [x]
lsdSortInts list = sortedNegs `par` (sortedPoss `pseq` (sortedNegs L.++ sortedPoss))
  where
    (poss, negs) = partBySign [] [] list
    sortedPoss = lsdRadixSort poss
    sortedNegs = lsdRadixSort negs
    

-- | sortNats, O(k n) where k= #digits
-- 
-- use this for Word<N> types
msdSortNats :: (RadixRep a) => [a] -> [a]
msdSortNats [] = []
msdSortNats [x] = [x]
msdSortNats list = msdRadixSort list

lsdSortNats :: (RadixRep a) => [a] -> [a]
lsdSortNats [] = []
lsdSortNats [x] = [x]
lsdSortNats list = lsdRadixSort list
