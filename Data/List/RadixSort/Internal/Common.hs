{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Data.List.RadixSort.Internal.Common (
  RadixRep(..), SignedQual(..), SortInfo(..),
  -- floatToWord, doubleToWord,
  (.$)
) where

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits

-- import Control.Exception (assert)

(.$) :: forall b c. b -> (b -> c) -> c
(.$) = flip ($)

---------------------------------------------------------
-- floatToWord type conversion from Jacob Stanley answer in
-- question http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-----------------------------------------------------------------

data SignedQual = Signed | Unsigned deriving (Eq, Show)

data SortInfo = SortInfo {siDigitSize::Int, siTopDigit::Int, siTopDigitVal::Int}

-- | class to instanciate for a type to be used in radix sorts
class RadixRep t where

  -- | obtain a word<N> representation, as for Float: toWordRep = fromIntegral . floatToWord
  toWordRep :: (Bits a, Integral a) => t -> a

  -- | size of the type in bits
  sizeOf :: t -> Int

  -- | Signed / Unsigned
  signedQual :: t -> SignedQual

instance RadixRep Float where
  toWordRep = fromIntegral . floatToWord
  sizeOf _ = 32
  signedQual _ = Signed

instance RadixRep Double where
  toWordRep = fromIntegral . doubleToWord
  sizeOf _ = 64
  signedQual _ = Signed

-------------------------------

instance RadixRep Int8 where
  toWordRep = fromIntegral
  sizeOf _ = 8
  signedQual _ = Signed

instance RadixRep Int16 where
  toWordRep = fromIntegral
  sizeOf _ = 16
  signedQual _ = Signed

instance RadixRep Int32 where
  toWordRep = fromIntegral
  sizeOf _ = 32
  signedQual _ = Signed

instance RadixRep Int64 where
  toWordRep = fromIntegral
  sizeOf _ = 64
  signedQual _ = Signed
  
{- Int representation may have bits reserved in compilers other than GHC

instance RadixRep Int where
  toWordRep = fromIntegral
  sizeOf x = bitSize (fromIntegral x ::Word)
  signedQual _ = Signed
  -}
  
-------------------------------

instance RadixRep Word8 where
  toWordRep = fromIntegral
  sizeOf _ = 8
  signedQual _ = Unsigned

instance RadixRep Word16 where
  toWordRep = fromIntegral
  sizeOf _ = 16
  signedQual _ = Unsigned

instance RadixRep Word32 where
  toWordRep = fromIntegral
  sizeOf _ = 32
  signedQual _ = Unsigned

instance RadixRep Word64 where
  toWordRep = fromIntegral
  sizeOf _ = 64
  signedQual _ = Unsigned

{- The Word type may have bits representation restricted to less bits. (GHC sets always the same number of bits as Int)

instance RadixRep Word where
  toWordRep = fromIntegral
  sizeOf = bitSize
  signedQual _ = Unsigned
  -}
------------------------------------------
