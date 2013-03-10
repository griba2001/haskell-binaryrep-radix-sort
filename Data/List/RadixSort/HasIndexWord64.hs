{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexWord64 (HasIndexWord64(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Word

-- | class to implement
class HasIndexWord64 t where
  indexWord64 :: t -> Word64

instance HasIndexWord64 t => RadixRep t where
  toWordRep = toWordRep . indexWord64
  sizeOf = sizeOf . indexWord64
  signedQual = signedQual . indexWord64
  repType = repType . indexWord64