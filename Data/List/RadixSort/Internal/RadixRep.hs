{-# LANGUAGE PackageImports, RecordWildCards #-}
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

getSortInfo :: (RadixRep a) => SortType -> a -> SortInfo
getSortInfo sortType x = SortInfo {
                         siDigitSize = bitsPerDigit,
                         siTopDigit = (sizeOf x) `div` bitsPerDigit - 1,
                         siTopDigitVal = bit bitsPerDigit -1,
                         siSigned = signedQual x,
                         siSize = sizeOf x,
                         siIsOrderReverse = False
                         }
  where
    bitsPerDigit = case sortType of
                        ST_LSD -> 8
                        ST_MSD -> 4

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
wordGetAllDigitVal sortInfo @ SortInfo {..} x =
        L.zip digitList bitsToShiftList
            .$ L.map (wordGetDigitVal sortInfo x)
  where
    digitList = [0..siTopDigit]
    bitsToShiftList = [0,siDigitSize..(siSize-siDigitSize)]

------------------------------------------

wordGetDigitVal :: (Bits a, Integral a) => SortInfo -> a -> (Int, Int) ->  Int
wordGetDigitVal SortInfo {..} bits (digit, bitsToShift) =
      assert (digit >= 0 && digit <= siTopDigit) $
        fromIntegral (shiftR (bits .&. mask) bitsToShift)
    where
      mask = if digit == siTopDigit && siSigned == Signed
              then shiftL digitMaskSignExcl bitsToShift
              else shiftL digitMask bitsToShift

      digitMask = bit siDigitSize -1
      digitMaskSignExcl = (bit (siDigitSize-1) -1)    -- sign excluded
