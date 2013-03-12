## Radix sort of numbers and numerically indexed data types, based on binary representation of primitive types

With msd prefix you have parallel Most significant digit radix sort.

With lsd prefix you have Least significant digit radix sort.

The Float and Double types have ordered representations within each sign subset.

The Int and Word types are not supported as [its binary representation may vary](http://www.haskell.org/ghc/docs/7.2.2/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html#g:1). You can use explicit length IntN and WordN types.

The flag --enable-tests builds the QuickCheck test-suite at (test/test.hs)

    cabal install --enable-tests

or

    cabal configure --enable-tests
    cabal build
    cabal test
    cabal haddock
    ...

You can sort lists of RadixRep instances (Float, Double, IntN, WordN) or, records with a mapping function to the types mentioned

It uses

* The __vector__ library to implement vectors
* The __Data.Sequence__ module to implement queues with constant O(1) access to both ends
* The __dlist__ package (difference lists) that have O(1) append costs to collect ordered sequences.

Actually both MSD and LSD methods run much slower than Data.List.sort (run the tests).

```haskell
    import Data.List.RadixSort.Base (msdSort, msdSortBy)

    import Data.List as L
    import System.Random
    import Control.Monad
    import Data.Ord

    data FRec = FRec {fieldF:: Float} deriving (Eq, Show)

    floatExample = do

        listF1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Float,100))

        let listF2 = L.map FRec listF1   -- :: [FRec]

        print listF1

        print $ msdSort listF1

        print $ msdSortBy fieldF listF2

    main = floatExample
```

