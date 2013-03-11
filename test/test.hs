
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

        putStrLn "-------------------"
        
        let len = 10000 :: Int
            (loExp, hiExp) = floatRange (1::Float)
            randomRFloat = randomR ((2^^(loExp-1))::Float,2^^(hiExp-1))
            randomRInt32 = randomR ((minBound::Int32),(maxBound::Int32))
            
        _ <- printf "\nComparison of times sorting a %d size list of Floats\n" len
        
        M.replicateM_ 5  
             ((M.replicateM len $ getStdRandom randomRFloat) >>= benchmark)

        _ <- printf "\n\nComparison of times sorting a %d size list of Int32\n" len
        
        M.replicateM_ 5
             ((M.replicateM len $ getStdRandom randomRInt32) >>= benchmark)
        
        putStrLn "-------------------"
        exitSuccess


benchmark :: (Ord a, RadixRep a) => [a] -> IO ()
benchmark list = do
        (t1, _s1) <- timed $ msdSort list
        (t2, _s2) <- timed $ lsdSort list
        (t3, _s3) <- timed $ L.sort list

        let tmin = min t1 (min t2 t3)

        putStr "\nmsdSort time: "
        putTimes t1 tmin

        putStr "lsdSort time: "
        putTimes t2 tmin

        putStr "Data.List time: "
        putTimes t3 tmin
  where
    putTimes t tmin = do
        putStr $ show t
        putStr "; ratio vs min. time: x"
        putStrLn $ show (t/tmin)
        