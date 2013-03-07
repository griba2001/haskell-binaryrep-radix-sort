{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.Util (
  partBySign,
  calcDigitSize,
  partListByDigit,
  collectVecToDList
) where

import Data.Bits
import Data.List.RadixSort.Internal.Common

import Data.Word (Word, Word8, Word16, Word32, Word64)
import Control.Exception (assert)
import Text.Printf (printf)

import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import "dlist" Data.DList (DList)
import qualified "dlist" Data.DList as D
import "vector" Data.Vector (Vector)
import qualified "vector" Data.Vector as V
import "vector" Data.Vector.Mutable (MVector)
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (ST)

------------------------------------------

------------------------------------------

-- partition by sign
partBySign :: (RadixRep a) => [a] -> [a] -> [a] -> ([a], [a])
partBySign poss negs [] = (poss, negs)
partBySign poss negs (x:xs) = if isNeg x
                               then partBySign poss (x:negs) xs
                               else partBySign (x:poss) negs xs
  where
        isNeg y = let s = sizeOf y
                  in case s of
                        64 -> testBit (toWordRep y :: Word64) (s-1)
                        32 -> testBit (toWordRep y :: Word32) (s-1)
                        16 -> testBit (toWordRep y :: Word16) (s-1)
                        8 ->  testBit (toWordRep y :: Word8) (s-1)
                        other -> error $ printf "size %d not supported!" other
                        
------------------------------------------

partListByDigit :: (RadixRep a) => SortInfo -> Int -> MVector s (Seq a) -> [a] -> ST s ()
partListByDigit sortInfo' digit' vec' list = do
        partListByDigitR bitsToShift' sortInfo' digit' vec' list
  where      
    bitsToShift' = digit' * (sortInfo' .$ siDigitSize)
    
    partListByDigitR _bitsToShift _sortInfo _digit _vec []  = return ()
    partListByDigitR bitsToShift sortInfo digit vec (x:xs) = do
        s <- VM.read vec digitVal
        VM.write vec digitVal (s S.|> x)
        partListByDigitR bitsToShift sortInfo digit vec xs
        return ()
      where
        digitVal = case sizeOf x of
                        64 -> wordGetDigitVal bitsToShift sortInfo signedQ digit $ (toWordRep x :: Word64)
                        32 -> wordGetDigitVal bitsToShift sortInfo signedQ digit $ (toWordRep x :: Word32)
                        16 -> wordGetDigitVal bitsToShift sortInfo signedQ digit $ (toWordRep x :: Word16)
                        8 -> wordGetDigitVal bitsToShift sortInfo signedQ digit $ (toWordRep x :: Word8)
                        other -> error $ printf "size %d not supported!" other

        signedQ = signedQual x

------------------------------------------

wordGetDigitVal :: (Bits a, Integral a) => Int -> SortInfo -> SignedQual -> Int -> a -> Int
wordGetDigitVal bitsToShift sortInfo signed digit bits =
      assert (digit >= 0 && digit <= topDigit) $ fromIntegral digitVal
    where
      digitVal = shiftR (bits .&. mask) bitsToShift
      
      mask = if digit == topDigit && signed == Signed
              then shiftL (fromIntegral digitMaskSignExcl) bitsToShift
              else shiftL (fromIntegral digitMask) bitsToShift
              
      digitMask = bit bitsPerDigit -1 :: Word
      digitMaskSignExcl = (bit (bitsPerDigit-1) -1) :: Word   -- sign excluded
      bitsPerDigit = sortInfo .$ siDigitSize
      topDigit = sortInfo .$ siTopDigit
        
------------------------------------------

collectVecToDList :: Vector (Seq a) -> Int -> DList a -> DList a
collectVecToDList vec n dl =
        if n == 0
           then new_accum_dl
           else collectVecToDList vec (n-1) new_accum_dl
      where
        new_accum_dl = dln `D.append` dl
        dln = D.fromList $ F.toList $ vec V.! n

------------------------------------------

calcDigitSize :: [a] -> Int
calcDigitSize _list = 8
  {-
        let (_prefix, postfix) = L.splitAt 500 list
        in if null postfix
                then 4  -- use small vectors (save space)
                else 8  -- use bigger vectors
                -}
