{-# LANGUAGE PackageImports #-}
-- | Least significant digit radix sort
module Data.List.RadixSort.Internal.LSD (lsdRadixSort) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.Util
import Data.List.RadixSort.Internal.RadixRep (getDigitVal)

-- import qualified Data.List as L
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (runST)
import Control.Exception (assert)
import qualified Control.Monad as M
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)

------------------------------------------

lsdRadixSort :: (RadixRep b) => (a -> b) -> SortInfo -> [Bool] -> [a] -> [a]
lsdRadixSort _indexMap _sortInfo _digitsConstancy [] = []
lsdRadixSort _indexMap _sortInfo _digitsConstancy [x] = [x]
lsdRadixSort indexMap sortInfo digitsConstancy list@(x:_) = assert (sizeOf (indexMap x) `mod` bitsPerDigit == 0) $ runST $ do
        
        vecIni <- V.thaw emptyVecOfSeqs
        -- partition by digit 0
        let digit' = 0
        if not $ digitsConstancy!!digit'
          then partListByDigit indexMap sortInfo digit' vecIni list
          else do -- constant data on digit 0, write list to digitVal pos.
                 let bitsToShift = 0
                     digitVal = getDigitVal sortInfo (indexMap x) digit' bitsToShift
                 VM.write vecIni digitVal $ S.fromList list    
        
        refVecFrom <- newSTRef vecIni

        M.when (topDigit > 0) $
           M.forM_ [1..topDigit] $ \digit -> do
             M.when ( not $ digitsConstancy!!digit)
                (do  -- sort by digit
                vecFrom <- readSTRef refVecFrom
                vecTo <- V.thaw emptyVecOfSeqs

                M.forM_ [0..topDigitVal] $ \digitVal -> do
                    -- read vecFrom queue
                    s <- VM.read vecFrom digitVal
                    -- partition to vecTo queues
                    partSeqByDigit indexMap sortInfo digit vecTo s

                writeSTRef refVecFrom vecTo
                )

        lastDigitSortedMVec <- readSTRef refVecFrom
        lastDigitSortedVec <- V.freeze lastDigitSortedMVec
        let dlist = collectVecToDList lastDigitSortedVec topDigitVal D.empty
        return $ D.toList dlist
  where

    emptyVecOfSeqs = V.replicate (topDigitVal+1) S.empty
    
    topDigitVal = sortInfo .$ siTopDigitVal
    topDigit = sortInfo .$ siTopDigit
    bitsPerDigit = sortInfo .$ siDigitSize

------------------------------------------
