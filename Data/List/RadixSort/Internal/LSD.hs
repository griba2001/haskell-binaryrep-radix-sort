{-# LANGUAGE PackageImports, RecordWildCards #-}
-- | Least significant digit radix sort
module Data.List.RadixSort.Internal.LSD (lsdRadixSort) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.Util
import Data.List.RadixSort.Internal.RadixRep (getDigitVal)

import qualified Data.Sequence as S
import "dlist" Data.DList (DList)
import "vector" Data.Vector (Vector)
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (runST)
import Control.Exception (assert)
import qualified Control.Monad as M
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)

------------------------------------------

lsdRadixSort :: (RadixRep b) => (a -> b) -> SortInfo -> Vector Bool -> [a] -> DList a
lsdRadixSort _indexMap _sortInfo _digitsConstancy [] = D.empty
lsdRadixSort _indexMap _sortInfo _digitsConstancy [x] = D.singleton x
lsdRadixSort indexMap sortInfo @ SortInfo {..} digitsConstancy list@(x:_) =
        assert (sizeOf (indexMap x) `mod` siDigitSize == 0) $ runST $ do
        
        vecIni <- V.unsafeThaw $ V.replicate (siTopDigitVal+1) S.empty
        -- partition by digit 0
        let digit' = 0
        if not $ digitsConstancy V.! digit'
          then partListByDigit indexMap sortInfo digit' vecIni list
          else do -- constant data on digit 0, write list to digitVal pos.
                 let bitsToShift = 0
                     digitVal = getDigitVal sortInfo (indexMap x) digit' bitsToShift
                 VM.write vecIni digitVal $ S.fromList list    
        
        refVecFrom <- newSTRef vecIni

        M.when (siTopDigit > 0) $ lsdRadixSortLoopWhile (< siTopDigit) refVecFrom 1

        readSTRef refVecFrom >>= V.unsafeFreeze >>= (return . collectVecToDList siTopDigitVal D.empty)

  where
    lsdRadixSortLoopWhile prop refVecFrom digit = do
            
             M.when ( not $ digitsConstancy V.! digit) $ do -- sort by digit
                vecFrom <- readSTRef refVecFrom
                vecTo <- V.thaw $ V.replicate (siTopDigitVal+1) S.empty
                
                readAndPartitionLoopWhile (< siTopDigitVal) digit vecFrom 0 vecTo

                writeSTRef refVecFrom vecTo
                
             if prop digit 
                then lsdRadixSortLoopWhile prop refVecFrom (digit +1)
                else return ()

    readAndPartitionLoopWhile prop digit vecFrom digitVal vecTo = do
                    -- read vecFrom queue
                    s <- VM.unsafeRead vecFrom digitVal
                    -- partition to vecTo queues
                    partSeqByDigit indexMap sortInfo digit vecTo s
                    
                    if prop digitVal
                       then readAndPartitionLoopWhile prop digit vecFrom (digitVal+1) vecTo
                       else return ()

------------------------------------------
