{-# LANGUAGE BangPatterns #-}
module Timed where

import Data.Time.Clock
import Control.Exception (evaluate)

timed :: a -> IO (NominalDiffTime, a)
timed expr = do
    startTime <- getCurrentTime
    res <- evaluate expr
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    return (elapsed, res)

timedIO :: IO a -> IO (NominalDiffTime, a)
timedIO io = do
    startTime <- getCurrentTime
    ! res <- io
    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
    return (elapsed, res)    