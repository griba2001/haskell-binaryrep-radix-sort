{-# LANGUAGE PackageImports, RecordWildCards #-}
-- | Least significant digit radix sort
module Data.List.RadixSort.Internal.LSD (lsdRadixSort) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.Common (partListByDigit, partSeqByDigit, collectVecToDList)
import Data.List.RadixSort.Internal.Util (forLoopUpM_)
import Data.List.RadixSort.Internal.RadixRep (getDigitVal)

import qualified Data.Sequence as S
import "dlist" Data.DList (DList)
import "vector" Data.Vector (Vector)
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Mutable as VM

import Control.Monad.ST (runST)
import Control.Exception (assert)
import qualified Control.Monad as M
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)

------------------------------------------

lsdRadixSort :: (RadixRep b) => (a -> b) -> SortInfo -> Vector Bool -> [a] -> DList a
lsdRadixSort _indexMap _sortInfo _digitsConstancy [] = D.empty
lsdRadixSort _indexMap _sortInfo _digitsConstancy [x] = D.singleton x
lsdRadixSort indexMap sortInfo @ SortInfo {..} digitsConstancy list@(x:_) =
        assert (sizeOf (indexMap x) `mod` siDigitSize == 0) $ runST $ do
                
        vecIni <- VM.replicate (siTopDigitVal+1) S.empty
        
        digitPassIni vecIni
        refVecFrom <- newSTRef vecIni

        M.when (siTopDigit > 0) $
            forLoopUpM_ (1::Int) (<= siTopDigit) $ \digit -> do
                M.when ( not $ digitsConstancy `V.unsafeIndex` digit) $ digitPass digit refVecFrom

        readSTRef refVecFrom >>= V.unsafeFreeze >>= (return . collectVecToDList siTopDigitVal D.empty)

  where
    -- partition by digit 0      
    digitPassIni vecIni = do  
        let digit = 0
        if not $ digitsConstancy `V.unsafeIndex` digit
          then partListByDigit indexMap sortInfo digit vecIni list
          else do -- constant data on digit 0, write list to digitVal pos.
                 let bitsToShift = 0
                     digitVal = getDigitVal sortInfo (indexMap x) digit bitsToShift
                 VM.write vecIni digitVal $ S.fromList list
                 
    -- sort by digit        
    digitPass digit refVecFrom = do 
                vecFrom <- readSTRef refVecFrom
                vecTo <- VM.replicate (siTopDigitVal+1) S.empty
                
                -- readAndPartitionForLoop 0 (<= siTopDigitVal) (+1) vecFrom digit vecTo
                forLoopUpM_ (0::Int) (<= siTopDigitVal) $ \digitVal -> do
                    -- read vecFrom queue
                    s <- VM.unsafeRead vecFrom digitVal
                    -- partition to vecTo queues
                    partSeqByDigit indexMap sortInfo digit vecTo s

                writeSTRef refVecFrom vecTo
             
------------------------------------------
