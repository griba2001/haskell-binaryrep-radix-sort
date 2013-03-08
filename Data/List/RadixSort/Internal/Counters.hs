{-# LANGUAGE PackageImports, CPP #-}
module Data.List.RadixSort.Internal.Counters (
  checkDigitsConstancy,
) where

-- import Data.Bits
import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.DigitVal (getAllDigitVals)

-- import qualified Data.List as L
import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import "vector" Data.Vector.Mutable (MVector)
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (ST)
import Control.Monad as M

#ifdef DEBUG
import Debug.Trace (trace)
#endif


------------------------------------------

checkDigitsConstancy :: (RadixRep a) => SortInfo -> [a] -> ST s [Bool]
checkDigitsConstancy sortInfo list = do

    vec <- V.replicateM (topDigit+1) $ VM.replicate (topDigitVal+1) (0::Int)
    
    len <- updateCounters sortInfo vec 0 list
    
    result <- M.forM [0..topDigit] $ \digit -> do
            let mvecCounters = vec V.! digit
            vecCounters <- V.freeze mvecCounters
            return $ V.any (== len) vecCounters
            
#ifdef DEBUG
    let result' = trace (show result) result
    return result'
#else
    return result
#endif

  where
        topDigit = sortInfo .$ siTopDigit
        topDigitVal = sortInfo .$ siTopDigitVal

-----------------------------

updateCounters :: (RadixRep a) => SortInfo -> Vector (MVector s Int) -> Int -> [a] -> ST s Int
updateCounters _sortInfo _vec cnt []  = return cnt
updateCounters sortInfo vec cnt (x:xs) = do
            
        M.forM_ [0..topDigit] $ \digit -> do
            let mvecCounters = vec V.! digit
            let digitVal = allDigitVals!!digit
            dvCnt <- VM.read mvecCounters digitVal
            VM.write mvecCounters digitVal (dvCnt +1)
            
        updateCounters sortInfo vec (cnt+1) xs
  where
        topDigit = sortInfo .$ siTopDigit
        allDigitVals = getAllDigitVals sortInfo x
  
