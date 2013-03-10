{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexFloat (HasIndexFloat(..)) where

import Data.List.RadixSort.Base (RadixRep(..))

-- | class to implement
class HasIndexFloat t where
  indexFloat :: t -> Float

instance HasIndexFloat t => RadixRep t where
  toWordRep = toWordRep . indexFloat
  sizeOf = sizeOf . indexFloat
  signedQual = signedQual . indexFloat
  repType = repType . indexFloat
