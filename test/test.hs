{-# LANGUAGE BangPatterns, TransformListComp #-}
import GHC.Exts (groupWith)  -- (the, groupWith, sortWith)
import Data.List.RadixSort.Base (msdSort, lsdSort, msdSortBy, lsdSortBy, RadixRep)

import Timed -- from test dir.


import Data.List as L
import Data.Int
import Data.Word
import Data.Ord
import Test.QuickCheck as QC
import System.Random
import System.Exit (exitSuccess, exitWith, ExitCode(..))
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
            

instance Arbitrary Rec where
  arbitrary = choose (Rec lo, Rec hi)
    where
      (lo, hi) = (2^^(loExp-1), 2^^(hiExp-1))
      (loExp, hiExp) = floatRange (1::Float)
            

checkOrdered :: (Ord a) => [a] -> Bool    -- Eq a, Num a,
checkOrdered [] = True
checkOrdered [_] = True
checkOrdered list = L.all orderedPair $ L.zip list (L.tail list)
  where
    orderedPair = uncurry (<=)

isQCSuccess :: QC.Result -> Bool    
isQCSuccess (QC.Success _ _ _) = True
isQCSuccess _ = False

deepCheck :: Testable prop => prop -> IO ()
deepCheck p = do
        res <- quickCheckWithResult (stdArgs { maxSuccess = 200, maxSize = 200}) p
        M.when (not $ isQCSuccess res) $ exitWith (ExitFailure 1)
        return ()

main :: IO a
main = do
        putStrLn "sorting by msd first [Float]"
        deepCheck ((\s -> let sorted = msdSort s in checkOrdered sorted && (length s == length sorted)) :: [Float] -> Bool)
             
        putStrLn "sorting by msd first [Double]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Double] -> Bool)


        putStrLn "sorting by msd first [Rec Float]"
        deepCheck ((\s -> checkOrdered $ msdSortBy fieldA s) :: [Rec] -> Bool)

        
        
        putStrLn "sorting by msd first [Int8]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Int8] -> Bool)

        
        putStrLn "sorting by msd first [Int16]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Int16] -> Bool)

        
        putStrLn "sorting by msd first [Int32]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Int32] -> Bool)

        
        putStrLn "sorting by msd first [Int64]"
        deepCheck ((\s -> let sorted = msdSort s in checkOrdered sorted && (length s == length sorted)) :: [Int64] -> Bool)


        putStrLn "sorting by msd first [Word8]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Word8] -> Bool)

        
        putStrLn "sorting by msd first [Word16]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Word16] -> Bool)

        
        putStrLn "sorting by msd first [Word32]"
        deepCheck ((\s -> checkOrdered $ msdSort s) :: [Word32] -> Bool)

        
        putStrLn "sorting by msd first [Word64]"
        deepCheck ((\s -> let sorted = msdSort s in checkOrdered sorted && (length s == length sorted)) :: [Word64] -> Bool)

        
        putStrLn "\n"
        
        putStrLn "sorting by lsd first [Float]"
        deepCheck ((\s -> let sorted = lsdSort s in checkOrdered sorted && (length s == length sorted)) :: [Float] -> Bool)

        
        putStrLn "sorting by lsd first [Double]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Double] -> Bool)


        putStrLn "sorting by lsd first [Rec Float]"
        deepCheck ((\s -> checkOrdered $ lsdSortBy fieldA s) :: [Rec] -> Bool)



        putStrLn "sorting by lsd first [Int8]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Int8] -> Bool)

        
        putStrLn "sorting by lsd first [Int16]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Int16] -> Bool)

        
        putStrLn "sorting by lsd first [Int32]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Int32] -> Bool)

        
        putStrLn "sorting by lsd first [Int64]"
        deepCheck ((\s -> let sorted = lsdSort s in checkOrdered sorted && (length s == length sorted)) :: [Int64] -> Bool)

        
        putStrLn "sorting by lsd first [Word8]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Word8] -> Bool)

        
        putStrLn "sorting by lsd first [Word16]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Word16] -> Bool)

        
        putStrLn "sorting by lsd first [Word32]"
        deepCheck ((\s -> checkOrdered $ lsdSort s) :: [Word32] -> Bool)

        
        putStrLn "sorting by lsd first [Word64]"
        deepCheck ((\s -> let sorted = lsdSort s in checkOrdered sorted && (length s == length sorted)) :: [Word64] -> Bool)

        putStrLn "\n-------------------"
        
        let len = 10000 :: Int
            (loExp, hiExp) = floatRange (1::Float)
            randomRFloat = randomR ((2^^(loExp-1))::Float,2^^(hiExp-1))
            randomRInt32 = randomR ((minBound::Int32),maxBound)
            
        _ <- printf "\nComparison of times sorting a %d size list of Floats\n" len
        
        times1 <- M.replicateM 5
             ((M.replicateM len $ getStdRandom randomRFloat) >>= benchmark1)
             
        let stats1 = [(avg msdTime, avg lsdTime, avg listTime)
                     | (grup, msdTime, lsdTime, listTime) <- times1
                     , then group by grup using groupWith
                     ]
                     
        benchmark1Show $ L.head stats1    

        _ <- printf "\n\nComparison of times sorting a %d size list of Int32\n" len
        
        times2 <- M.replicateM 5
             ((M.replicateM len $ getStdRandom randomRInt32) >>= benchmark2)

        let stats2 = [(avg msdTime, avg lsdTime, avg listTime, avg vectorTime)
                     | (grup, msdTime, lsdTime, listTime, vectorTime) <- times2
                     , then group by grup using groupWith
                     ]
                     
        benchmark2Show $ L.head stats2
        
        putStrLn "\n-------------------"
        exitSuccess


benchmark1 :: (Ord a, RadixRep a) => [a] -> IO (Int, NominalDiffTime, NominalDiffTime, NominalDiffTime)
benchmark1 list = do
        (t1, _s1) <- timed $ msdSort list
        (t2, _s2) <- timed $ lsdSort list
        (t3, _s3) <- timed $ L.sort list
        return (1, t1, t2, t3)

benchmark1Show :: (NominalDiffTime, NominalDiffTime, NominalDiffTime) -> IO ()
benchmark1Show (t1, t2, t3) = do
        let tmin = min t1 (min t2 t3)

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


benchmark2 :: (Ord a, RadixRep a, VAR.Radix a) => [a] -> IO (Int, NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime)
benchmark2 list = do
        (t1, _s1) <- timed $ msdSort list
        (t2, _s2) <- timed $ lsdSort list
        (t3, _s3) <- timed $ L.sort list
        
        let ! v1 = V.fromList list
        (t4, _s4) <- timedIO $ do
                vec <- V.unsafeThaw v1
                VAR.sort vec
                V.unsafeFreeze vec
        return (1, t1, t2, t3, t4)

benchmark2Show :: (NominalDiffTime, NominalDiffTime, NominalDiffTime, NominalDiffTime) -> IO ()        
benchmark2Show (t1, t2, t3, t4) = do
        let tmin = min t1 (min t2 (min t3 t4))

        putStr "\n1. msdSort avg time: "
        putTimes t1 tmin

        putStr "2. lsdSort avg time: "
        putTimes t2 tmin

        putStr "3. Data.List.sort avg time: "
        putTimes t3 tmin

        putStr "4. Data.Vector.Algorithms.Radix.sort avg time: "
        putTimes t4 tmin
        
avg :: (Fractional a, Eq a) => [a] -> a
avg llista = avg_acum llista 0 0
  where
        avg_acum [] recompte suma
                             | recompte == 0  = 0
                             | otherwise      = suma / recompte

        avg_acum (x:xs) recompte suma = avg_acum xs (recompte +1) (suma +x)        