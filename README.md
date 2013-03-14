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
-------------------

Performance:

It uses:

* The __vector__ library to implement vectors
* The __Data.Sequence__ module to implement queues with constant O(1) access to both ends
* The __dlist__ package (difference lists) that have O(1) append costs to collect ordered sequences.

Actually both MSD and LSD methods __run much slower__ than Data.List.sort (the test includes a benchmark).

Test output:

Test suite test-binaryrep-radix-sort: RUNNING...
sorting by msd first [Float]
+++ OK, passed 200 tests.
sorting by msd first [Double]
+++ OK, passed 200 tests.
sorting by msd first [Rec Float]
+++ OK, passed 200 tests.
sorting by msd first [Int8]
+++ OK, passed 200 tests.
sorting by msd first [Int16]
+++ OK, passed 200 tests.
sorting by msd first [Int32]
+++ OK, passed 200 tests.
sorting by msd first [Int64]
+++ OK, passed 200 tests.
sorting by msd first [Word8]
+++ OK, passed 200 tests.
sorting by msd first [Word16]
+++ OK, passed 200 tests.
sorting by msd first [Word32]
+++ OK, passed 200 tests.
sorting by msd first [Word64]
+++ OK, passed 200 tests.


sorting by lsd first [Float]
+++ OK, passed 200 tests.
sorting by lsd first [Double]
+++ OK, passed 200 tests.
sorting by lsd first [Rec Float]
+++ OK, passed 200 tests.
sorting by lsd first [Int8]
+++ OK, passed 200 tests.
sorting by lsd first [Int16]
+++ OK, passed 200 tests.
sorting by lsd first [Int32]
+++ OK, passed 200 tests.
sorting by lsd first [Int64]
+++ OK, passed 200 tests.
sorting by lsd first [Word8]
+++ OK, passed 200 tests.
sorting by lsd first [Word16]
+++ OK, passed 200 tests.
sorting by lsd first [Word32]
+++ OK, passed 200 tests.
sorting by lsd first [Word64]
+++ OK, passed 200 tests.

-------------------

Comparison of times sorting a 10000 size list of Floats

1. msdSort avg time: 0.286683s; ratio vs min. avg time: x13.420860446608s
2. lsdSort avg time: 0.510584s; ratio vs min. avg time: x23.902626281541s
3. Data.List.sort avg time: 0.021361s; ratio vs min. avg time: x1s


Comparison of times sorting a 10000 size list of Int32

1. msdSort avg time: 0.2006304s; ratio vs min. avg time: x25.416844025539s
2. lsdSort avg time: 0.1276496s; ratio vs min. avg time: x16.171277997364s
3. Data.List.sort avg time: 0.0121444s; ratio vs min. avg time: x1.538512212425s
4. Data.Vector.Algorithms.Radix.sort avg time: 0.0078936s; ratio vs min. avg time: x1s

-------------------
Test suite test-binaryrep-radix-sort: PASS
