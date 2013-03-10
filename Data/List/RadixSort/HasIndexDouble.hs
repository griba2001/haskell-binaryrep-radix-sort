{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances #-} --

module Data.List.RadixSort.HasIndexDouble (HasIndexDouble(..)) where

import Data.List.RadixSort.Base (RadixRep(..))

-- | class to implement
class HasIndexDouble t where
  indexDouble :: t -> Double

instance HasIndexDouble t => RadixRep t where
  toWordRep = toWordRep . indexDouble
  sizeOf = sizeOf . indexDouble
  signedQual = signedQual . indexDouble
  repType = repType . indexDouble
