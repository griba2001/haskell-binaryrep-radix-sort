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
-- import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import Text.Printf (printf)
------------------------------------------

getSortInfo :: (RadixRep a) => SortType -> a -> SortInfo
getSortInfo sortType x = SortInfo {
                         siDigitSize = bitsPerDigit,
                         siTopDigit = topDigit,
                         siTopDigitVal = bit bitsPerDigit -1,
                         siSize = size,
                         siIsOrderReverse = False,
                         siIsSigned = isRRSigned,
                         siDigitsWithBitsToShift = digitsWithBitsToShift,        
                         siMasks = V.fromList maskList 
                         }
  where
    bitsPerDigit = case sortType of
                        ST_LSD -> 8
                        ST_MSD -> 8
    size = sizeOf x                    
    topDigit = (sizeOf x) `div` bitsPerDigit - 1
    isRRSigned = repType x /= RT_WordN
    digitsWithBitsToShift = L.zip bitsToShiftList digitList
    maskList = L.map (uncurry (getDigitMask bitsPerDigit isRRSigned topDigit)) digitsWithBitsToShift
                   
    digitList = [0..topDigit]
    bitsToShiftList = [0,bitsPerDigit..(size-bitsPerDigit)]


getDigitMask :: Int -> Bool -> Int -> Int -> Int -> Word64
getDigitMask bitsPerDigit isRRSigned topDigit bitsToShift digit =
        if digit == topDigit && isRRSigned
              then shiftL digitMaskSignExcl bitsToShift
              else shiftL digitMask bitsToShift
  where
      digitMask = bit bitsPerDigit -1
      digitMaskSignExcl = (bit (bitsPerDigit-1) -1)    -- sign excluded

------------------------------------------

isNeg ::  (RadixRep a) => a -> Bool
isNeg y = if repType y == RT_WordN
             then False
             else let s = sizeOf y
                  in case s of
                        64 -> testBit (toWordRep y :: Word64) (s-1)
                        32 -> testBit (toWordRep y :: Word32) (s-1)
                        16 -> testBit (toWordRep y :: Word16) (s-1)
                        8 ->  testBit (toWordRep y :: Word8) (s-1)
                        other -> error $ printf "size %d not supported!" other
{-# INLINABLE isNeg #-}

------------------------------------------

getDigitVal :: (RadixRep a) => SortInfo -> a -> Int -> Int -> Int
getDigitVal sortInfo x digit bitsToShift =

        case sizeOf x of
                        64 -> wordGetDigitVal sortInfo (toWordRep x :: Word64) bitsToShift digit
                        32 -> wordGetDigitVal sortInfo (toWordRep x :: Word32) bitsToShift digit
                        16 -> wordGetDigitVal sortInfo (toWordRep x :: Word16) bitsToShift digit
                        8 -> wordGetDigitVal sortInfo (toWordRep x :: Word8) bitsToShift digit
                        other -> error $ printf "size %d not supported!" other
{-# INLINABLE getDigitVal #-}

------------------------------------------

getAllDigitVals :: (RadixRep a) => SortInfo -> a -> [Int]
getAllDigitVals sortInfo x =
        
        case sizeOf x of
                        64 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word64)
                        32 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word32)
                        16 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word16)
                        8 -> wordGetAllDigitVal sortInfo (toWordRep x :: Word8)
                        other -> error $ printf "size %d not supported!" other
{-# INLINABLE getAllDigitVals #-}
                        
------------------------------------------

wordGetAllDigitVal :: (Bits a, Integral a) => SortInfo -> a -> [Int]
wordGetAllDigitVal sortInfo @ SortInfo {..} x =
    L.map (uncurry (wordGetDigitVal sortInfo x)) siDigitsWithBitsToShift
{-# INLINE wordGetAllDigitVal #-}

------------------------------------------
wordGetDigitVal :: (Bits a, Integral a) => SortInfo -> a -> Int -> Int ->  Int
wordGetDigitVal SortInfo {..} bits bitsToShift digit =
      assert (digit >= 0 && digit <= siTopDigit) 
        (fromIntegral (shiftR (bits .&. mask) bitsToShift)
        )
    where
      mask = fromIntegral (siMasks `V.unsafeIndex` digit)
{-# INLINABLE wordGetDigitVal #-}


