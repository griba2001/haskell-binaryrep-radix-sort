{-# LANGUAGE PackageImports, RecordWildCards #-}
module Data.List.RadixSort.Internal.Util (
  xor,
  forLoopM_,
  forLoopUpM_, forLoopDownM_
) where

import Control.Monad as M
                       
------------------------------------------

xor :: Bool -> Bool -> Bool
xor False y = y
xor True y = not y
{-# INLINE xor #-}

------------------------------------------

forLoopM_ :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoopM_ indx prop incr f = do
        f indx
        M.when (prop next) $ forLoopM_ next prop incr f
  where      
    next = incr indx    
{-# INLINABLE forLoopM_ #-}
        
------------------------------------------
forLoopUpM_ :: (Monad m, Enum a) => a -> (a -> Bool) -> (a -> m ()) -> m ()
forLoopUpM_ indx prop f = forLoopM_ indx prop succ f
{-# INLINE forLoopUpM_ #-}

forLoopDownM_ :: (Monad m, Enum a) => a -> (a -> Bool) -> (a -> m ()) -> m ()
forLoopDownM_ indx prop f = forLoopM_ indx prop pred f
{-# INLINE forLoopDownM_ #-}


