{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.DigitVal (
  wordGetAllDigitVal,
  wordGetDigitVal        
) where

import Data.Bits
import Data.List.RadixSort.Internal.Common

import Control.Exception (assert)

import qualified Data.List as L

------------------------------------------

wordGetAllDigitVal :: (Bits a, Integral a) => SortInfo -> a -> [Int]
wordGetAllDigitVal sortInfo x =
        L.zip digitList bitsToShiftList
            .$ L.map (wordGetDigitVal sortInfo x)
  where
    digitList = [0..topDigit]
    bitsToShiftList = [0,bitsPerDigit..(size-bitsPerDigit)]
    topDigit = sortInfo .$ siTopDigit
    size = sortInfo .$ siSize
    bitsPerDigit = sortInfo .$ siDigitSize

------------------------------------------

wordGetDigitVal :: (Bits a, Integral a) => SortInfo -> a -> (Int, Int) ->  Int
wordGetDigitVal sortInfo bits (digit, bitsToShift) =
      assert (digit >= 0 && digit <= topDigit) $ fromIntegral digitVal
    where
      digitVal = shiftR (bits .&. mask) bitsToShift

      mask = if digit == topDigit && signed == Signed
              then shiftL digitMaskSignExcl bitsToShift
              else shiftL digitMask bitsToShift

      digitMask = bit bitsPerDigit -1
      digitMaskSignExcl = (bit (bitsPerDigit-1) -1)    -- sign excluded
      bitsPerDigit = sortInfo .$ siDigitSize
      topDigit = sortInfo .$ siTopDigit
      signed = sortInfo .$ siSigned
