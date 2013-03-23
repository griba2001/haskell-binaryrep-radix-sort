{-# LANGUAGE RankNTypes, PackageImports, FlexibleContexts #-}
module Data.List.RadixSort.Internal.Types (
  RadixRep(..), SortInfo(..), RepType(..), SortType(..),
  (.$)
) where

import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Bits
import "vector" Data.Vector (Vector)
-- import qualified "vector" Data.Vector as V

-- import Control.Exception (assert)

(.$) :: forall b c. b -> (b -> c) -> c
(.$) = flip ($)

---------------------------------------------------------
-- floatToWord type conversion from Jacob Stanley answer in
-- question http://stackoverflow.com/questions/6976684/converting-ieee-754-floating-point-in-haskell-word32-64-to-and-from-haskell-floa

floatToWord :: Float -> Word32
floatToWord x = runST (cast x)
{-# INLINE floatToWord #-}

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)
{-# INLINE doubleToWord #-}

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

-----------------------------------------------------------------

data SortInfo = SortInfo {siDigitSize:: {-# UNPACK #-} !Int,
                          siTopDigit:: {-# UNPACK #-} !Int,
                          siTopDigitVal:: {-# UNPACK #-} !Int,
                          siIsSigned:: !Bool,
                          siSize:: {-# UNPACK #-} !Int,
                          siIsOrderReverse:: !Bool,
                          siMasks :: !(Vector Word64),
                          siDigitsWithBitsToShift :: ![(Int, Int)]
                          }

data RepType = RT_Float | RT_IntN | RT_WordN deriving Eq

data SortType = ST_LSD | ST_MSD deriving Eq

-- | class to instanciate for a type to be used in radix sorts
class (Ord t) => RadixRep t where

  -- | obtain a word<N> representation, as for Float: toWordRep = fromIntegral . floatToWord
  toWordRep :: (Bits a, Integral a) => t -> a

  -- | size of the type in bits
  sizeOf :: t -> Int

  -- | Representation type
  repType :: t -> RepType

instance RadixRep Float where
  toWordRep = fromIntegral . floatToWord
  {-# INLINE toWordRep #-}
  sizeOf _ = 32
  {-# INLINE sizeOf #-}
  repType _ = RT_Float
  {-# INLINE repType #-}

instance RadixRep Double where
  toWordRep = fromIntegral . doubleToWord
  {-# INLINE toWordRep #-}
  sizeOf _ = 64
  {-# INLINE sizeOf #-}
  repType _ = RT_Float
  {-# INLINE repType #-}

-------------------------------

instance RadixRep Int8 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  repType _ = RT_IntN
  {-# INLINE repType #-}

instance RadixRep Int16 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  repType _ = RT_IntN
  {-# INLINE repType #-}

instance RadixRep Int32 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 32
  {-# INLINE sizeOf #-}
  repType _ = RT_IntN
  {-# INLINE repType #-}

instance RadixRep Int64 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 64
  {-# INLINE sizeOf #-}
  repType _ = RT_IntN
  {-# INLINE repType #-}
  
{- Int representation may have bits reserved in compilers other than GHC

instance RadixRep Int where
  toWordRep = fromIntegral
  sizeOf x = bitSize (fromIntegral x ::Word)
  repType _ = RT_IntN
  -}
  
-------------------------------

instance RadixRep Word8 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 8
  {-# INLINE sizeOf #-}
  repType _ = RT_WordN
  {-# INLINE repType #-}

instance RadixRep Word16 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  repType _ = RT_WordN
  {-# INLINE repType #-}

instance RadixRep Word32 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 32
  {-# INLINE sizeOf #-}
  repType _ = RT_WordN
  {-# INLINE repType #-}

instance RadixRep Word64 where
  toWordRep = fromIntegral
  {-# INLINE toWordRep #-}
  sizeOf _ = 64
  {-# INLINE sizeOf #-}
  repType _ = RT_WordN
  {-# INLINE repType #-}

{- The Word type may have bits representation restricted to less bits. (GHC sets always the same number of bits as Int)

instance RadixRep Word where
  toWordRep = fromIntegral
  sizeOf = bitSize
  repType _ = RT_WordN
  -}
------------------------------------------
