{-# LANGUAGE PackageImports, CPP #-}
-- | Most significant digit radix sort
module Data.List.RadixSort.Internal.MSD (msdRadixSort) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.Util
import Data.List.RadixSort.Internal.Counters (checkDigitsConstancy)

import Data.Bits
import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import "dlist" Data.DList (DList)
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V

import GHC.ST (runST)
import Control.Exception (assert)
import "parallel" Control.Parallel.Strategies

#ifdef DEBUG
import Debug.Trace (trace)
#endif

------------------------------------------

sortByDigit :: (RadixRep a) => SortInfo -> [Bool] -> Int -> [a] -> DList a
sortByDigit _sortInfo _digitsConstancy _digit [] = D.empty
sortByDigit _sortInfo _digitsConstancy _digit [x] = D.singleton x

sortByDigit sortInfo digitsConstancy digit list = runST $ do
        mvec <- V.thaw emptyVecOfSeqs
        -- partition by digit
        partListByDigit sortInfo digit mvec list
        vec <- V.freeze mvec
        let nextDigit = nextSortableDigit digitsConstancy digit
        if digit == 0 || nextDigit < 0 
           then do
                return $ collectVecToDList vec topDigitVal D.empty
                
           else do
                let dlists = vec .$ V.toList
                                 .$ map F.toList
                                 .$ parMap rseq (sortByDigit sortInfo digitsConstancy nextDigit)
                                 
                return $ D.concat dlists                      
  where
    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    topDigitVal = sortInfo .$ siTopDigitVal

------------------------------------------
nextSortableDigit :: [Bool] -> Int -> Int
nextSortableDigit digitsConstancy digit = (digit - 1 - digitsToSkip')
  where
          
#ifdef DEBUG
    digitsToSkip' = trace ("msdSort digitsToSkip: " ++ show digit ++ show digitsConstancy ++ show digitsToSkip) digitsToSkip
#else
    digitsToSkip' = digitsToSkip 
#endif

    digitsToSkip = L.length $ L.takeWhile (== True) msdDigitsConstancy    
    msdDigitsConstancy = L.take digit digitsConstancy .$ L.reverse

------------------------------------------
       
msdRadixSort :: (RadixRep a) => [a] -> [a]
msdRadixSort [] = []
msdRadixSort [x] = [x]
msdRadixSort list = assert (sizeOf (head list) `mod` bitsPerDigit == 0) $ runST $ do
        
   digitsConstancy <- checkDigitsConstancy sortInfo list
   let nextDigit = nextSortableDigit digitsConstancy (topDigit+1)
   let sortedList =
        if nextDigit >= 0   
           then list .$ sortByDigit sortInfo digitsConstancy nextDigit
                         .$ D.toList
           else list
           
   return sortedList       
  where
    sortInfo = SortInfo {siDigitSize = bitsPerDigit,
                         siTopDigit = topDigit,
                         siTopDigitVal = topDigitVal,
                         siSigned = signedQual (L.head list),
                         siSize = sizeOf (L.head list)
                         }
    topDigit = (sizeOf $ L.head list) `div` bitsPerDigit - 1
    topDigitVal = bit bitsPerDigit -1
    bitsPerDigit = calcDigitSize list
          
        
