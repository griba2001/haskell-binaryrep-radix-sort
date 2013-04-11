{-# LANGUAGE PackageImports #-}

-- {-# LANGUAGE PackageImports, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-} -- , OverlappingInstances
{- | Radix sort of lists, based on binary representation of

       * floats
       
       * Int<N>
       
       * Word<N> 

  The lsd prefix is for Least significant digit radix sort, while msd is for the parallelized Most significant digit one.

  Here we partition numbers by sign and sort both lists in parallel (you should link with -threaded)

  The digit size is set to 8 bits.

  Digit value queues ('Seq' a) are appended as difference lists ('DList' from package dlist, O(1) on append)

  Instances of RadixRep for 'Int' and 'Word' are not supported.
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
  msdSortBy,
  lsdSortBy,
  RadixRep(..)
) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.MSD (msdRadixSort)
import Data.List.RadixSort.Internal.LSD (lsdRadixSort)
import Data.List.RadixSort.Internal.Counters (countAndPartBySign)
import Data.List.RadixSort.Internal.RadixRep (getSortInfo)

import qualified Data.List as L
-- import "dlist" Data.DList (DList)
import qualified "dlist" Data.DList as D
import "parallel" Control.Parallel.Strategies (using, rpar, rseq)
import GHC.ST (runST)

-- | A first pass to build digit counters is used to split lists by sign, then they are sorted in parallel.
-- 
-- Each split group is sparked to compute in parallel
-- 
-- Worst case O((k+1) n + length negatives (append)) where k= #digits.

msdSort :: (RadixRep a) => [a] -> [a]
msdSort = msdSortBy id

msdSortBy :: (RadixRep b) => (a -> b) -> [a] -> [a]
msdSortBy _indexMap [] = []
msdSortBy _indexMap [x] = [x]
msdSortBy indexMap list@(x:_) = case repType rep of
                           RT_Float -> msdSortFloats sortInfo indexMap list
                           RT_IntN -> msdSortInts sortInfo indexMap list
                           RT_WordN -> msdSortNats sortInfo indexMap list
  where
    sortInfo = getSortInfo ST_MSD rep
    rep = indexMap x

-- | A first pass to build digit counters is used to split lists by sign, then they are sorted in parallel
--
-- Worst case O((k+1) n + length negatives (append)) where k= #digits.

lsdSort :: (RadixRep a) => [a] -> [a]
lsdSort = lsdSortBy id 

lsdSortBy :: (RadixRep b) => (a -> b) -> [a] -> [a]
lsdSortBy _indexMap [] = []
lsdSortBy _indexMap [x] = [x]
lsdSortBy indexMap list@(x:_) = case repType rep of
                           RT_Float -> lsdSortFloats sortInfo indexMap list
                           RT_IntN -> lsdSortInts sortInfo indexMap list
                           RT_WordN -> lsdSortNats sortInfo indexMap list
  where
    sortInfo = getSortInfo ST_MSD rep
    rep = indexMap x

----------------------------------                           
                           
msdSortFloats :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
msdSortFloats sortInfo indexMap list = (sortedNegs `using` rpar) `prependReversing` (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign indexMap sortInfo list
    sortedPoss = D.toList $ msdRadixSort indexMap sortInfo digitsConstPos poss
    sortedNegs = D.toList $ msdRadixSort indexMap sortInfo {siIsOrderReverse = True} digitsConstNeg negs

prependReversing :: [a] -> [a] -> [a]
prependReversing negs poss  = L.foldl' (flip (:)) poss negs
{-# INLINABLE prependReversing #-}


lsdSortFloats :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
lsdSortFloats sortInfo indexMap list = (sortedNegs `using` rpar) `prependReversing` (sortedPoss `using` rseq)
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign indexMap sortInfo list
    sortedPoss = D.toList $ lsdRadixSort indexMap sortInfo digitsConstPos poss
    sortedNegs = D.toList $ msdRadixSort indexMap sortInfo {siIsOrderReverse = True} digitsConstNeg negs

----------------------------------
                      
msdSortInts :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
msdSortInts sortInfo indexMap list = D.toList $ D.concat [(sortedNegs `using` rpar), (sortedPoss `using` rseq)]
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign indexMap sortInfo list
    sortedPoss = msdRadixSort indexMap sortInfo digitsConstPos poss
    sortedNegs = msdRadixSort indexMap sortInfo digitsConstNeg negs

lsdSortInts :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
lsdSortInts sortInfo indexMap list = D.toList $ D.concat [(sortedNegs `using` rpar), (sortedPoss `using` rseq)]
  where
    (poss, digitsConstPos, negs, digitsConstNeg) = runST $ countAndPartBySign indexMap sortInfo list
    sortedPoss = lsdRadixSort indexMap sortInfo digitsConstPos poss
    sortedNegs = lsdRadixSort indexMap sortInfo digitsConstNeg negs
    
----------------------------------

msdSortNats :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
msdSortNats sortInfo indexMap list = D.toList $ msdRadixSort indexMap sortInfo digitsConstPos poss
  where
    (poss, digitsConstPos, _, _) = runST $ countAndPartBySign indexMap sortInfo list
          

lsdSortNats :: (RadixRep b) => SortInfo -> (a -> b) -> [a] -> [a]
lsdSortNats sortInfo indexMap list = D.toList $ lsdRadixSort indexMap sortInfo digitsConstPos poss
  where
    (poss, digitsConstPos, _, _) = runST $ countAndPartBySign indexMap sortInfo list
