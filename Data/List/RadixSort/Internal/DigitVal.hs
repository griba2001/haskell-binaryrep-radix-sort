{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.DigitVal (
  -- wordGetAllDigitVal,
  -- wordGetDigitVal,
  getDigitVal,
  getAllDigitVals        
) where

import Data.Bits
import Data.List.RadixSort.Internal.Common

import Control.Exception (assert)

import qualified Data.List as L
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Printf (printf)


------------------------------------------

getDigitVal :: (RadixRep a) => SortInfo -> a -> Int -> Int -> Int
getDigitVal sortInfo x digit bitsToShift =

        case sizeOf x of
                        64 -> wordGetDigitVal sortInfo (toWordRep x :: Word64) (digit, bitsToShift)
                        32 -> wordGetDigitVal sortInfo (toWordRep x :: Word32) (digit, bitsToShift)
                        16 -> wordGetDigitVal sortInfo (toWordRep x :: Word16) (digit, bitsToShift)
                        8 -> wordGetDigitVal sortInfo (toWordRep x :: Word8) (digit, bitsToShift)
                        other -> error $ printf "size %d not supported!" other

------------------------------------------

getAllDigitVals :: (RadixRep a) => SortInfo -> a -> [Int]
getAllDigitVals sortInfo x =
        
        case sizeOf x of
                        64 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word64)
                        32 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word32)
                        16 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word16)
                        8 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word8)
                        other -> error $ printf "size %d not supported!" other
                        
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
