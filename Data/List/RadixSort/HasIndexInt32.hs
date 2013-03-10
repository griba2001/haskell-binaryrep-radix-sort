{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexInt32 (HasIndexInt32(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Int

-- | class to implement
class HasIndexInt32 t where
  indexInt32 :: t -> Int32

instance HasIndexInt32 t => RadixRep t where
  toWordRep = toWordRep . indexInt32
  sizeOf = sizeOf . indexInt32
  signedQual = signedQual . indexInt32
  repType = repType . indexInt32