{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexWord8 (HasIndexWord8(..)) where

import Data.List.RadixSort.Base (RadixRep(..))
import Data.Word

-- | class to implement
class HasIndexWord8 t where
  indexWord8 :: t -> Word8

instance HasIndexWord8 t => RadixRep t where
  toWordRep = toWordRep . indexWord8
  sizeOf = sizeOf . indexWord8
  signedQual = signedQual . indexWord8
  repType = repType . indexWord8