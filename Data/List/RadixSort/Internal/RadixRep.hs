{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.RadixRep (
  getSortInfo,
  isNeg,
  getDigitVal,
  getAllDigitVals,
) where

import Data.Bits
import Data.List.RadixSort.Internal.Types

import Control.Exception (assert)

import qualified Data.List as L
import Data.Word (Word8, Word16, Word32, Word64)
import Text.Printf (printf)
------------------------------------------

getSortInfo :: (RadixRep a) => a -> SortInfo
getSortInfo x = SortInfo {siDigitSize = bitsPerDigit,
                         siTopDigit = topDigit,
                         siTopDigitVal = topDigitVal,
                         siSigned = signedQual x,
                         siSize = sizeOf x,
                         siIsOrderReverse = False
                         }
  where
    topDigitVal = bit bitsPerDigit -1
    topDigit = (sizeOf x) `div` bitsPerDigit - 1
    bitsPerDigit = 8

------------------------------------------

isNeg ::  (RadixRep a) => a -> Bool
isNeg y = if signedQual y == Unsigned
             then False
             else let s = sizeOf y
                  in case s of
                        64 -> testBit (toWordRep y :: Word64) (s-1)
                        32 -> testBit (toWordRep y :: Word32) (s-1)
                        16 -> testBit (toWordRep y :: Word16) (s-1)
                        8 ->  testBit (toWordRep y :: Word8) (s-1)
                        other -> error $ printf "size %d not supported!" other

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
