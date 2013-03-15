{-# LANGUAGE BangPatterns #-}

import Data.List.RadixSort.Base (msdSort, lsdSort, RadixRep)  -- , msdSortBy, lsdSortBy

import Timed -- from test dir.


import Data.List as L
import Data.Int
-- import Data.Word
import Data.Ord
import System.Random
import System.Exit (exitSuccess)    -- , exitWith, ExitCode(..)
import Control.Monad as M
import Text.Printf
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Radix as VAR
import Data.Time.Clock


data Rec = Rec {fieldA:: Float} deriving (Eq, Show)

instance Ord Rec where
  compare = comparing fieldA      

instance Random Rec where
  randomR (Rec lo, Rec hi)  g = (Rec x, g')
    where
      (x, g') = randomR (lo, hi) g
      
  random g = (Rec x, g')
    where
      (x, g') = random g
            

main :: IO a
main = do
        let len = 10000 :: Int
            (loExp, hiExp) = floatRange (1::Float)
            randomRFloat = randomR ((2^^(loExp-1))::Float,2^^(hiExp-1))
            randomRInt32 = randomR ((minBound::Int32),maxBound)
            
        _ <- printf "\nComparison of times sorting a %d size list of Floats\n" len
        
        times1 <- M.replicateM 5
             ((M.replicateM len $ getStdRandom randomRFloat) >>= benchmark1)
             
        do let (lt1, lt2, lt3) = unzip3 times1
               (t1, t2, t3) = (avg lt1, avg lt2, avg lt3)
               tmin = L.minimum [t1, t2, t3]
           benchmark1Show tmin t1 t2 t3

        _ <- printf "\n\nComparison of times sorting a %d size list of Int32\n" len
        
        times2 <- M.replicateM 5
             ((M.replicateM len $ getStdRandom randomRInt32) >>= benchmark2)

        do let (lt1, lt2, lt3, lt4) = unzip4 times2
               (t1, t2, t3, t4) = (avg lt1, avg lt2, avg lt3, avg lt4)
               tmin = L.minimum [t1, t2, t3, t4]
            
           benchmark1Show tmin t1 t2 t3
           benchmark2Show tmin t4
        
        putStrLn "\n-------------------"
        exitSuccess


benchmark1 :: (Ord a, RadixRep a) => [a] -> IO (NominalDiffTime, NominalDiffTime, NominalDiffTime)
benchmark1 list = do
        (t1, _s1) <- timed $ msdSort list
        (t2, _s2) <- timed $ lsdSort list
        (t3, _s3) <- timed $ L.sort list
        return (t1, t2, t3)

benchmark1Show :: NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> NominalDiffTime -> IO ()
benchmark1Show tmin t1 t2 t3 = do

        putStr "\n1. msdSort avg time: "
        putTimes t1 tmin

        putStr "2. lsdSort avg time: "
        putTimes t2 tmin

        putStr "3. Data.List.sort avg time: "
        putTimes t3 tmin

putTimes :: NominalDiffTime -> NominalDiffTime -> IO ()
putTimes t tmin = do
        putStr $ show t
        putStr "; ratio vs min. avg time: x"
        putStrLn $ show (t/tmin)


benchmark2 :: (Ord a, RadixRep a, VAR.Radix a) => [a] -> IO (NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime)
benchmark2 list = do
        (t1, _s1) <- timed $ msdSort list
        (t2, _s2) <- timed $ lsdSort list
        (t3, _s3) <- timed $ L.sort list
        
        let ! v1 = V.fromList list
        (t4, _s4) <- timedIO $ do
                vec <- V.unsafeThaw v1
                VAR.sort vec
                V.unsafeFreeze vec
        return (t1, t2, t3, t4)

benchmark2Show :: NominalDiffTime -> NominalDiffTime -> IO ()
benchmark2Show tmin t4 = do
        putStr "4. Data.Vector.Algorithms.Radix.sort avg time: "
        putTimes t4 tmin
        
avg :: (Fractional a, Eq a) => [a] -> a
avg llista = avg_acum llista 0 0
  where
        avg_acum [] recompte suma
                             | recompte == 0  = 0
                             | otherwise      = suma / recompte

        avg_acum (x:xs) recompte suma = avg_acum xs (recompte +1) (suma +x)        