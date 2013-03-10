{-# LANGUAGE PackageImports, CPP #-}
module Data.List.RadixSort.Internal.Counters (
  countAndPartBySign
) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.RadixRep (getAllDigitVals, isNeg)

import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import "vector" Data.Vector.Mutable (MVector)
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (ST)
import Control.Monad as M

------------------------------------------

countAndPartBySign :: (RadixRep a) => SortInfo -> [a] -> ST s ([a], [Bool], [a], [Bool])
countAndPartBySign sortInfo list = do

    vecPos <- V.replicateM (topDigit+1) $ VM.replicate (topDigitVal+1) (0::Int)
    vecNeg <- V.replicateM (topDigit+1) $ VM.replicate (topDigitVal+1) (0::Int)
    
    (lenPos, poss, lenNeg, negs) <- updateCounters sortInfo vecPos 0 [] vecNeg 0 [] list
    
    digitsConstPos <- M.forM [0..topDigit] $ \digit -> do
            let mvecCounters = vecPos V.! digit
            vecCounters <- V.freeze mvecCounters
            return $ V.any (== lenPos) vecCounters

    digitsConstNeg <- M.forM [0..topDigit] $ \digit -> do
            let mvecCounters = vecNeg V.! digit
            vecCounters <- V.freeze mvecCounters
            return $ V.any (== lenNeg) vecCounters

    return (poss, digitsConstPos, negs, digitsConstNeg)
            
  where
        topDigit = sortInfo .$ siTopDigit
        topDigitVal = sortInfo .$ siTopDigitVal

-----------------------------

updateCounters :: (RadixRep a) => SortInfo -> Vector (MVector s Int) -> Int -> [a] ->
                                              Vector (MVector s Int) -> Int -> [a] ->
                                              [a] -> ST s (Int, [a], Int, [a])
updateCounters _sortInfo _vecPos cntPos accumPos _vecNeg cntNeg accumNeg []  = return (cntPos, accumPos, cntNeg, accumNeg)
updateCounters sortInfo vecPos cntPos accumPos vecNeg cntNeg accumNeg (x:xs) = do
            
        M.forM_ [0..topDigit] $ \digit -> do
            let mvecCounters = if isNeg x
                                   then vecNeg V.! digit
                                   else vecPos V.! digit
                                   
            let digitVal = allDigitVals!!digit
            dvCnt <- VM.read mvecCounters digitVal
            VM.write mvecCounters digitVal (dvCnt +1)

        if isNeg x
           then updateCounters sortInfo vecPos cntPos accumPos vecNeg (cntNeg+1) (x:accumNeg) xs
           else updateCounters sortInfo vecPos (cntPos+1) (x:accumPos) vecNeg cntNeg accumNeg xs
  where
        topDigit = sortInfo .$ siTopDigit
        allDigitVals = getAllDigitVals sortInfo x
  
