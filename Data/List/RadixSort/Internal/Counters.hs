{-# LANGUAGE PackageImports, CPP, BangPatterns, RecordWildCards #-}
module Data.List.RadixSort.Internal.Counters (
  countAndPartBySign
) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.RadixRep (getAllDigitVals, isNeg)

import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import "vector" Data.Vector.Mutable (MVector)
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (ST)
import Control.Monad as M

------------------------------------------

countAndPartBySign :: (RadixRep b) => (a -> b) -> SortInfo -> [a] -> ST s ([a], [Bool], [a], [Bool])
countAndPartBySign indexMap sortInfo @ SortInfo {..} list = do

    vecPos <- V.replicateM (siTopDigit+1) $ VM.replicate (siTopDigitVal+1) (0::Int)
    vecNeg <- V.replicateM (siTopDigit+1) $ VM.replicate (siTopDigitVal+1) (0::Int)
    
    (lenPos, poss, lenNeg, negs) <- updateCounters indexMap sortInfo vecPos 0 [] vecNeg 0 [] list
    
    digitsConstPos <- M.forM [0..siTopDigit] $ \digit -> do
            let mvecCounters = vecPos V.! digit
            vecCounters <- V.freeze mvecCounters
            return $ V.any (== lenPos) vecCounters

    digitsConstNeg <- M.forM [0..siTopDigit] $ \digit -> do
            let mvecCounters = vecNeg V.! digit
            vecCounters <- V.freeze mvecCounters
            return $ V.any (== lenNeg) vecCounters

    return (poss, digitsConstPos, negs, digitsConstNeg)
            
-----------------------------

updateCounters :: (RadixRep b) => (a -> b) -> SortInfo ->
                  Vector (MVector s Int) -> Int -> [a] ->
                  Vector (MVector s Int) -> Int -> [a] ->
                                                   [a] -> ST s (Int, [a], Int, [a])
updateCounters _indexMap _sortInfo _vecPos cntPos accumPos _vecNeg cntNeg accumNeg []  = return (cntPos, accumPos, cntNeg, accumNeg)
updateCounters indexMap sortInfo @ SortInfo {..} vecPos cntPos accumPos vecNeg cntNeg accumNeg (x:xs) = do
            
        M.forM_ [0..siTopDigit] $ \digit -> do
            let mvecCounters = if isNegIndexVal
                                   then vecNeg V.! digit
                                   else vecPos V.! digit
                                   
            let digitVal = allDigitVals!!digit
            dvCnt <- VM.unsafeRead mvecCounters digitVal
            VM.unsafeWrite mvecCounters digitVal (dvCnt +1)

        if isNegIndexVal
           then updateCounters indexMap sortInfo vecPos cntPos accumPos vecNeg (cntNeg+1) (x:accumNeg) xs
           else updateCounters indexMap sortInfo vecPos (cntPos+1) (x:accumPos) vecNeg cntNeg accumNeg xs
  where
        ! allDigitVals = getAllDigitVals sortInfo indexVal
        indexVal = indexMap x
        isNegIndexVal = isNeg indexVal
  
