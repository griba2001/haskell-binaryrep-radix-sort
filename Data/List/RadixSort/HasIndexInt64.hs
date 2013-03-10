{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexInt64 (HasIndexInt64(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Int

-- | class to implement
class HasIndexInt64 t where
  indexInt64 :: t -> Int64

instance HasIndexInt64 t => RadixRep t where
  toWordRep = toWordRep . indexInt64
  sizeOf = sizeOf . indexInt64
  signedQual = signedQual . indexInt64
  repType = repType . indexInt64