{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE PackageImports, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-} -- , OverlappingInstances
{- | Radix sort (O(k n) where k= #digits) of lists of floats (based on its IEEE754 representation) or Int<N> or Word<N> based on their representation

  The lsd prefix is for Least significant digit radix sort, while msd is for the parallelized Most significant digit one.

  Here we partition numbers by sign and sort both lists in parallel (you should link with -threaded)

  The digit size is set to 8 bits.

  digit value queues are appended as difference lists (DList from package dlist, O(1) on append)

  Instances for 'Int' and 'Word' are not supported.
  The instance for 'Int' (machine word size) is not portable because it may have reserved bits for compiler use.
  The type Word may be restricted to the same number of bits as Int.
  Check The word size story at <http://www.haskell.org/ghc/docs/7.2.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html#g:1>

  A quickcheck test-suite has been added.
-}
-- @author: Gabriel Riba Faura
-- Internally uses (.$) = flip ($)

module Data.List.RadixSort.Base (
  msdSort,
  lsdSort,
  RadixRep(..)
) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.MSD (msdRadixSort)
import Data.List.RadixSort.Internal.LSD (lsdRadixSort)
import Data.List.RadixSort.Internal.Counters (countAndPartBySign)
import Data.List.RadixSort.Internal.RadixRep (getSortInfo)

import qualified Data.List as L
import "parallel" Control.Parallel.Strategies (using, rpar, rseq)
import GHC.ST (runST)

-- | O((2k+1) n) where k= #digits.

msdSort :: (RadixRep a) => [a] -> [a]
msdSort[] = []
msdSort[x] = [x]
msdSort list@(x:_) = case repType x of
                           RT_Float -> msdSortFloats list
                           RT_IntN -> msdSortInts list
                           RT_WordN -> msdSortNats list

-- | O((k+1) n) where k= #digits.
                           
lsdSort :: (RadixRep a) => [a] -> [a]
lsdSort[] = []
lsdSort[x] = [x]
lsdSort list@(x:_) = case repType x of
                           RT_Float -> lsdSortFloats list
                           RT_IntN -> lsdSortInts list
                           RT_WordN -> lsdSortNats list
                           
msdSortFloats :: (RadixRep a) => [a] -> [a]
msdSortFloats [] = []
msdSortFloats [x] = [x]
msdSortFloats list = (sortedNegs `using` rpar) L.++ (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
    sortedPoss = msdRadixSort sortInfo digitsConstPos poss
    sortedNegs = negs .$ msdRadixSort sortInfo digitsConstNeg
                      .$ L.reverse


lsdSortFloats :: (RadixRep a) => [a] -> [a]
lsdSortFloats [] = []
lsdSortFloats [x] = [x]
lsdSortFloats list = (sortedNegs `using` rpar) L.++ (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
    sortedPoss = lsdRadixSort sortInfo digitsConstPos poss
    sortedNegs = negs .$ lsdRadixSort sortInfo digitsConstNeg
                      .$ L.reverse
                      
msdSortInts :: (RadixRep a) => [a] -> [a]
msdSortInts [] = []
msdSortInts [x] = [x]
msdSortInts list = (sortedNegs `using` rpar) L.++ (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
    sortedPoss = msdRadixSort sortInfo digitsConstPos poss
    sortedNegs = msdRadixSort sortInfo digitsConstNeg negs

lsdSortInts :: (RadixRep a) => [a] -> [a]
lsdSortInts [] = []
lsdSortInts [x] = [x]
lsdSortInts list = (sortedNegs `using` rpar) L.++ (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
    sortedPoss = lsdRadixSort sortInfo digitsConstPos poss
    sortedNegs = lsdRadixSort sortInfo digitsConstNeg negs
    

msdSortNats :: (RadixRep a) => [a] -> [a]
msdSortNats [] = []
msdSortNats [x] = [x]
msdSortNats list = msdRadixSort sortInfo digitsConstPos poss
  where
    (poss, digitsConstPos, _, _) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
          

lsdSortNats :: (RadixRep a) => [a] -> [a]
lsdSortNats [] = []
lsdSortNats [x] = [x]
lsdSortNats list = lsdRadixSort sortInfo digitsConstPos poss
  where
    (poss, digitsConstPos, _, _) = runST $ countAndPartBySign sortInfo list
    sortInfo = getSortInfo $ head list
