{-# LANGUAGE PackageImports #-}
module Data.List.RadixSort.Internal.Util (
  partListByDigit,
  collectVecToDList,
) where

import Data.List.RadixSort.Internal.Common
import Data.List.RadixSort.Internal.RadixRep (getDigitVal)

import Data.Bits
-- import qualified Data.List as L
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
        digitVal = getDigitVal sortInfo x digit bitsToShift
        
        
------------------------------------------

collectVecToDList :: Vector (Seq a) -> Int -> DList a -> DList a
collectVecToDList vec n dl =
        if n == 0
           then new_accum_dl
           else collectVecToDList vec (n-1) new_accum_dl
      where
        new_accum_dl = dln `D.append` dl
        dln = D.fromList $ F.toList $ vec V.! n

                
