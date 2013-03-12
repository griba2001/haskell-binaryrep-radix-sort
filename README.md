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

Actually both MSD and LSD methods __RUN MUCH SLOWER__ than Data.List.sort (the test includes a benchmark).


Comparison of times sorting a 10000 size list of Floats (replicated 5 times)

1. msdSort time: 0.227957s; ratio vs min. time: x16.005968262884s
1. lsdSort time: 0.36944s; ratio vs min. time: x25.94017694144s
1. Data.List.sort time: 0.014242s; ratio vs min. time: x1s

-----------

1. msdSort time: 0.249989s; ratio vs min. time: x17.004897626011s
1. lsdSort time: 0.396428s; ratio vs min. time: x26.966056730834s
1. Data.List.sort time: 0.014701s; ratio vs min. time: x1s

-----------

1. msdSort time: 0.214817s; ratio vs min. time: x15.400172055344s
1. lsdSort time: 0.397558s; ratio vs min. time: x28.500824431858s
1. Data.List.sort time: 0.013949s; ratio vs min. time: x1s

-----------

1. msdSort time: 0.213596s; ratio vs min. time: x13.821405461369s
1. lsdSort time: 0.377318s; ratio vs min. time: x24.415555843147s
1. Data.List.sort time: 0.015454s; ratio vs min. time: x1s

-----------

1. msdSort time: 0.257411s; ratio vs min. time: x12.765869867089s
1. lsdSort time: 0.423192s; ratio vs min. time: x20.987502479666s
1. Data.List.sort time: 0.020164s; ratio vs min. time: x1s

----------

Comparison of times sorting a 10000 size list of Int32 (replicated 5 times)

1. msdSort time: 0.29914s; ratio vs min. time: x28.098816456885s
1. lsdSort time: 0.211813s; ratio vs min. time: x19.896017283486s
1. Data.List.sort time: 0.018008s; ratio vs min. time: x1.69152733421s
1. Data.Vector.Algorithms.Radix.sort time: 0.010646s; ratio vs min. time: x1s

---------------

1. msdSort time: 0.303674s; ratio vs min. time: x28.781537295043s
1. lsdSort time: 0.205365s; ratio vs min. time: x19.464031845322s
1. Data.List.sort time: 0.022134s; ratio vs min. time: x2.097810634063s
1. Data.Vector.Algorithms.Radix.sort time: 0.010551s; ratio vs min. time: x1s

---------------

1. msdSort time: 0.323617s; ratio vs min. time: x32.961601140761s
1. lsdSort time: 0.15866s; ratio vs min. time: x16.160114076186s
1. Data.List.sort time: 0.018238s; ratio vs min. time: x1.857608474231s
1. Data.Vector.Algorithms.Radix.sort time: 0.009818s; ratio vs min. time: x1s

---------------

1. msdSort time: 0.206807s; ratio vs min. time: x28.895766382562s
1. lsdSort time: 0.116238s; ratio vs min. time: x16.241162498253s
1. Data.List.sort time: 0.009274s; ratio vs min. time: x1.295794327232s
1. Data.Vector.Algorithms.Radix.sort time: 0.007157s; ratio vs min. time: x1s

---------------

1. msdSort time: 0.193588s; ratio vs min. time: x26.705476617464s
1. lsdSort time: 0.110426s; ratio vs min. time: x15.233273554973s
1. Data.List.sort time: 0.012184s; ratio vs min. time: x1.680783556352s
1. Data.Vector.Algorithms.Radix.sort time: 0.007249s; ratio vs min. time: x1s
