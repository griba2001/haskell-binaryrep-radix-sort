{-# LANGUAGE PackageImports, RecordWildCards #-}
module Data.List.RadixSort.Internal.Util (
  partListByDigit, partSeqByDigit,
  collectVecToDList,
          xor, forLoopM,  forLoopM_
) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.RadixRep (getDigitVal)

-- import Data.Bits
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
import Control.Monad as M
                       
------------------------------------------

partListByDigit :: (RadixRep b) => (a -> b) -> SortInfo -> Int -> MVector s (Seq a) -> [a] -> ST s ()
partListByDigit indexMap sortInfo' @ SortInfo {..} digit' vec' list = do
        partListByDigitR bitsToShift' sortInfo' digit' vec' list
  where      
    bitsToShift' = digit' * siDigitSize
    
    partListByDigitR _bitsToShift _sortInfo _digit _vec []  = return ()
    partListByDigitR bitsToShift sortInfo digit vec (x:xs) = do
            
        s <- VM.unsafeRead vec digitVal
        VM.unsafeWrite vec digitVal (s S.|> x)
        
        partListByDigitR bitsToShift sortInfo digit vec xs
      where
        digitVal = getDigitVal sortInfo (indexMap x) digit bitsToShift
        
------------------------------------------

partSeqByDigit :: (RadixRep b) => (a -> b) -> SortInfo -> Int -> MVector s (Seq a) -> Seq a -> ST s ()
partSeqByDigit indexMap sortInfo' @ SortInfo {..} digit' vec' sq = do
        partSeqByDigitR bitsToShift' sortInfo' digit' vec' (S.viewl sq)
  where
    bitsToShift' = digit' * siDigitSize

    partSeqByDigitR _bitsToShift _sortInfo _digit _vec S.EmptyL  = return ()
    partSeqByDigitR bitsToShift sortInfo digit vec (x S.:< xs) = do
            
        s <- VM.unsafeRead vec digitVal
        VM.unsafeWrite vec digitVal (s S.|> x)
        
        partSeqByDigitR bitsToShift sortInfo digit vec (S.viewl xs)
      where
        digitVal = getDigitVal sortInfo (indexMap x) digit bitsToShift
        
------------------------------------------

collectVecToDList :: Int -> DList a -> Vector (Seq a) -> DList a
collectVecToDList n dl vec =
        if n == 0
           then new_accum_dl
           else collectVecToDList (n-1) new_accum_dl vec
      where
        new_accum_dl = dln `D.append` dl
        dln = D.fromList $ F.toList $ vec V.! n
{-# INLINABLE collectVecToDList #-}

------------------------------------------

xor :: Bool -> Bool -> Bool
xor False True = True
xor True False = True
xor _ _ = False
{-# INLINE xor #-}

------------------------------------------

forLoopM_ :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoopM_ indx prop incr f = do
        f indx
        M.when (prop next) $ forLoopM_ next prop incr f
  where      
    next = incr indx    
        
------------------------------------------
forLoopM :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m b) -> m [b]
forLoopM indx prop incr f = forLoopMR indx
  where
    forLoopMR indx' = do
        (y, next) <- f' indx'
        ys <- if prop next
                 then forLoopMR next
                 else return []
        return (y:ys)
        
    f' indx' = do
            res <- f indx'
            return (res, incr indx')

