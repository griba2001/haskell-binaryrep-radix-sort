{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexWord16 (HasIndexWord16(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Word

-- | class to implement
class HasIndexWord16 t where
  indexWord16 :: t -> Word16

instance HasIndexWord16 t => RadixRep t where
  toWordRep = toWordRep . indexWord16
  sizeOf = sizeOf . indexWord16
  signedQual = signedQual . indexWord16
  repType = repType . indexWord16