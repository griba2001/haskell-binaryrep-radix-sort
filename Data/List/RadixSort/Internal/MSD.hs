{-# LANGUAGE PackageImports, CPP, RecordWildCards, BangPatterns #-}
-- | Most significant digit radix sort
module Data.List.RadixSort.Internal.MSD (msdRadixSort) where

import Data.List.RadixSort.Internal.Types
import Data.List.RadixSort.Internal.Util (partSeqByDigit, collectVecToDList, xor)

import qualified Data.List as L
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import "dlist" Data.DList (DList)
import "vector" Data.Vector (Vector)
import qualified "dlist" Data.DList as D
import qualified "vector" Data.Vector as V
import qualified "vector" Data.Vector.Mutable as VM

import GHC.ST (runST)
import Control.Exception (assert)
import "parallel" Control.Parallel.Strategies

#ifdef DEBUG
import Debug.Trace (trace)
#endif

------------------------------------------

sortByDigit :: (RadixRep b) => (a -> b) -> SortInfo -> Vector Bool -> Int -> (Seq a) -> DList a
sortByDigit indexMap sortInfo @ SortInfo {..} digitsConstancy digit sq =
    case S.viewl sq of
      S.EmptyL -> D.empty
      (x S.:< xs) ->
        if S.null xs
           then D.singleton x
           else runST $ do
                mvec <- VM.replicate (siTopDigitVal+1) S.empty
                -- partition by digit
                partSeqByDigit indexMap sortInfo digit mvec sq
                vec <- V.unsafeFreeze mvec
                let nextDigit = nextSortableDigit digitsConstancy digit
                if digit == 0 || nextDigit < 0
                then do
                        return $ collectVecToDList siTopDigitVal D.empty vec

                else do
                        let dlists = vec .$ V.toList
                                         .$ L.map (recSort nextDigit)

                        return $ D.concat dlists
  where
    recSort nextDigit sq' =
            case S.viewl sq' of
              S.EmptyL -> D.empty
              (x S.:< xs) ->
                 case S.viewl xs of
                   S.EmptyL -> D.singleton x
                   (y S.:< ys) ->
                     if S.null ys
                        then D.fromList $ order2 x y
                        else -- three or more elements
                             (sortByDigit indexMap sortInfo digitsConstancy nextDigit sq') `using` rpar  -- spark it in parallel
                        
    order2 x y = if (indexMap x <= indexMap y) `xor` siIsOrderReverse
                                   then [x, y]
                                   else [y, x]
            
    

------------------------------------------
nextSortableDigit :: Vector Bool -> Int -> Int
nextSortableDigit digitsConstancy digit = nextDigit - digitsToSkip'
  where
          
#ifdef DEBUG
    digitsToSkip' = trace ("msdSort digitsToSkip: " ++ show digit
                           ++ show digitsConstancy ++ show digitsToSkip) digitsToSkip
#else
    digitsToSkip' = digitsToSkip 
#endif

    nextDigit = digit -1

    digitsToSkip = reverseCountWhile (==True) nextDigit 0
            
    reverseCountWhile prop indx cnt =
            
      let propHolds = prop $ digitsConstancy V.! indx
          newCnt = if propHolds then cnt +1 else cnt
      in
         if propHolds && indx > 0
           then reverseCountWhile prop (indx -1) newCnt
           else newCnt
           
------------------------------------------
       
msdRadixSort :: (RadixRep b) => (a -> b) -> SortInfo -> Vector Bool -> [a] ->  DList a
msdRadixSort _indexMap _sortInfo _digitsConstancy [] = D.empty
msdRadixSort _indexMap _sortInfo _digitsConstancy [x] = D.singleton x
msdRadixSort indexMap sortInfo @ SortInfo {..} digitsConstancy list@(x:_) =
        assert (sizeOf (indexMap x) `mod` siDigitSize == 0) $ 
  if nextDigit >= 0
                    then sortByDigit indexMap sortInfo digitsConstancy nextDigit $ S.fromList list
                    else D.fromList list
  where
    nextDigit = nextSortableDigit digitsConstancy (siTopDigit+1)      
        
