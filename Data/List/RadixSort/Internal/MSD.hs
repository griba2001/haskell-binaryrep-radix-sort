{-# LANGUAGE PackageImports, CPP #-}
-- | Most significant digit radix sort
module Data.List.RadixSort.Internal.MSD (msdRadixSort) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.Util

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

sortByDigit :: (RadixRep b) => (a -> b) -> SortInfo -> [Bool] -> Int -> [a] -> DList a
sortByDigit _indexMap _sortInfo _digitsConstancy _digit [] = D.empty
sortByDigit _indexMap _sortInfo _digitsConstancy _digit [x] = D.singleton x

sortByDigit indexMap sortInfo digitsConstancy digit list = runST $ do
        mvec <- V.thaw emptyVecOfSeqs
        -- partition by digit
        partListByDigit indexMap sortInfo digit mvec list
        vec <- V.freeze mvec
        let nextDigit = nextSortableDigit digitsConstancy digit
        if digit == 0 || nextDigit < 0 
           then do
                return $ collectVecToDList vec topDigitVal D.empty
                
           else do
                let dlists = vec .$ V.toList
                                 .$ map F.toList
                                 .$ map (recSort nextDigit)
                                 
                return $ D.concat dlists                      
  where
    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    topDigitVal = sortInfo .$ siTopDigitVal
    
    recSort _nextDigit [] = D.empty
    recSort _nextDigit [x] = D.singleton x
    recSort _nextDigit [x, y] = if (indexMap x <= indexMap y) `xor` isOrderReverse
                                   then D.fromList [x, y]
                                   else D.fromList [y, x]
                                   
    recSort nextDigit list' = (sortByDigit indexMap sortInfo digitsConstancy nextDigit list') `using` rpar  -- spark it in parallel

    isOrderReverse = sortInfo .$ siIsOrderReverse
    

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
       
msdRadixSort :: (RadixRep b) => (a -> b) -> SortInfo -> [Bool] -> [a] -> [a]
msdRadixSort _indexMap _sortInfo _digitsConstancy [] = []
msdRadixSort _indexMap _sortInfo _digitsConstancy [x] = [x]
msdRadixSort indexMap sortInfo digitsConstancy list@(x:_) = assert (sizeOf (indexMap x) `mod` bitsPerDigit == 0) sortedList
        
  where
    sortedList = if nextDigit >= 0
                    then D.toList returnList
                    else list
    returnList = sortByDigit indexMap sortInfo digitsConstancy nextDigit list
    nextDigit = nextSortableDigit digitsConstancy (topDigit+1)      
    bitsPerDigit = sortInfo .$ siDigitSize
    topDigit = sortInfo .$ siTopDigit
          
        
