{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexInt16 (HasIndexInt16(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Int

-- | class to implement
class HasIndexInt16 t where
  indexInt16 :: t -> Int16

instance HasIndexInt16 t => RadixRep t where
  toWordRep = toWordRep . indexInt16
  sizeOf = sizeOf . indexInt16
  signedQual = signedQual . indexInt16
  repType = repType . indexInt16