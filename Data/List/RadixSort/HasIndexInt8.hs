{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexInt8 (HasIndexInt8(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Int

-- | class to implement
class HasIndexInt8 t where
  indexInt8 :: t -> Int8

instance HasIndexInt8 t => RadixRep t where
  toWordRep = toWordRep . indexInt8
  sizeOf = sizeOf . indexInt8
  signedQual = signedQual . indexInt8
  repType = repType . indexInt8