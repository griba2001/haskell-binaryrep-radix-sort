{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexWord32 (HasIndexWord32(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Word

-- | class to implement
class HasIndexWord32 t where
  indexWord32 :: t -> Word32

instance HasIndexWord32 t => RadixRep t where
  toWordRep = toWordRep . indexWord32
  sizeOf = sizeOf . indexWord32
  signedQual = signedQual . indexWord32
  repType = repType . indexWord32