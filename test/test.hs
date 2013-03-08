
import Data.List.RadixSort.Base (msdSortInts, msdSortFloats, msdSortNats,
                                 lsdSortInts, lsdSortFloats, lsdSortNats,
                                 RadixRep)
import Data.List.RadixSort.HasIndexFloat (HasIndexFloat(..))
import Data.List as L
import Data.Int
import Data.Word
import Data.Ord
import Test.QuickCheck as QC
import System.Random
import System.Exit (exitSuccess, exitWith, ExitCode(..))
import Control.Monad (when)

data Rec = Rec {fieldA:: Float} deriving (Eq, Show)

instance HasIndexFloat Rec where
  indexFloat = fieldA

instance Ord Rec where
  compare = comparing indexFloat

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
            

checkOrdered :: (Ord a, RadixRep a) => [a] -> Bool    -- Eq a, Num a, 
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
        res <- quickCheckWithResult (stdArgs { maxSuccess = 100, maxSize = 200}) p
        when (not $ isQCSuccess res) $ exitWith (ExitFailure 1)
        return ()

main :: IO a
main = do
        putStrLn "sorting by msd first [Float]"
        deepCheck ((\s -> checkOrdered $ msdSortFloats s) :: [Float] -> Bool)

             
        putStrLn "sorting by msd first [Double]"
        deepCheck ((\s -> checkOrdered $ msdSortFloats s) :: [Double] -> Bool)


        putStrLn "sorting by msd first [Rec Float]"
        deepCheck ((\s -> checkOrdered $ msdSortFloats s) :: [Rec] -> Bool)

        
        
        putStrLn "sorting by msd first [Int8]"
        deepCheck ((\s -> checkOrdered $ msdSortInts s) :: [Int8] -> Bool)

        
        putStrLn "sorting by msd first [Int16]"
        deepCheck ((\s -> checkOrdered $ msdSortInts s) :: [Int16] -> Bool)

        
        putStrLn "sorting by msd first [Int32]"
        deepCheck ((\s -> checkOrdered $ msdSortInts s) :: [Int32] -> Bool)

        
        putStrLn "sorting by msd first [Int64]"
        deepCheck ((\s -> checkOrdered $ msdSortInts s) :: [Int64] -> Bool)


        putStrLn "sorting by msd first [Word8]"
        deepCheck ((\s -> checkOrdered $ msdSortNats s) :: [Word8] -> Bool)

        
        putStrLn "sorting by msd first [Word16]"
        deepCheck ((\s -> checkOrdered $ msdSortNats s) :: [Word16] -> Bool)

        
        putStrLn "sorting by msd first [Word32]"
        deepCheck ((\s -> checkOrdered $ msdSortNats s) :: [Word32] -> Bool)

        
        putStrLn "sorting by msd first [Word64]"
        deepCheck ((\s -> checkOrdered $ msdSortNats s) :: [Word64] -> Bool)

        
        putStrLn "\n"
        
        putStrLn "sorting by lsd first [Float]"
        deepCheck ((\s -> checkOrdered $ lsdSortFloats s) :: [Float] -> Bool)

        
        putStrLn "sorting by lsd first [Double]"
        deepCheck ((\s -> checkOrdered $ lsdSortFloats s) :: [Double] -> Bool)


        putStrLn "sorting by lsd first [Rec Float]"
        deepCheck ((\s -> checkOrdered $ lsdSortFloats s) :: [Rec] -> Bool)



        putStrLn "sorting by lsd first [Int8]"
        deepCheck ((\s -> checkOrdered $ lsdSortInts s) :: [Int8] -> Bool)

        
        putStrLn "sorting by lsd first [Int16]"
        deepCheck ((\s -> checkOrdered $ lsdSortInts s) :: [Int16] -> Bool)

        
        putStrLn "sorting by lsd first [Int32]"
        deepCheck ((\s -> checkOrdered $ lsdSortInts s) :: [Int32] -> Bool)

        
        putStrLn "sorting by lsd first [Int64]"
        deepCheck ((\s -> checkOrdered $ lsdSortInts s) :: [Int64] -> Bool)

        
        putStrLn "sorting by lsd first [Word8]"
        deepCheck ((\s -> checkOrdered $ lsdSortNats s) :: [Word8] -> Bool)

        
        putStrLn "sorting by lsd first [Word16]"
        deepCheck ((\s -> checkOrdered $ lsdSortNats s) :: [Word16] -> Bool)

        
        putStrLn "sorting by lsd first [Word32]"
        deepCheck ((\s -> checkOrdered $ lsdSortNats s) :: [Word32] -> Bool)

        
        putStrLn "sorting by lsd first [Word64]"
        deepCheck ((\s -> checkOrdered $ lsdSortNats s) :: [Word64] -> Bool)

        
        exitSuccess