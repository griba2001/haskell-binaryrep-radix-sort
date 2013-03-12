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

Benchmarks replicated 5 times:

Comparison of times sorting a 10000 size list of Floats ^

1. msdSort time: 0.256787s; ratio vs min. time: x15.696986368359s
1. lsdSort time: 0.478728s; ratio vs min. time: x29.263891435906s
1. Data.List.sort time: 0.016359s; ratio vs min. time: x1s

---------

1. msdSort time: 0.251979s; ratio vs min. time: x16.070089285714s
1. lsdSort time: 0.449266s; ratio vs min. time: x28.652168367346s
1. Data.List.sort time: 0.01568s; ratio vs min. time: x1s

---------

1. msdSort time: 0.238799s; ratio vs min. time: x21.069260631727s
1. lsdSort time: 0.441168s; ratio vs min. time: x38.924298570672s
1. Data.List.sort time: 0.011334s; ratio vs min. time: x1s

---------

1. msdSort time: 0.263264s; ratio vs min. time: x17.541577825159s
1. lsdSort time: 0.430236s; ratio vs min. time: x28.6671108742s
1. Data.List.sort time: 0.015008s; ratio vs min. time: x1s

---------

1. msdSort time: 0.256446s; ratio vs min. time: x12.48337633257s
1. lsdSort time: 0.460808s; ratio vs min. time: x22.431387820668s
1. Data.List.sort time: 0.020543s; ratio vs min. time: x1s


---------

Comparison of times sorting a 10000 size list of Int32

1. msdSort time: 0.215055s; ratio vs min. time: x29.761278715748s
1. lsdSort time: 0.124551s; ratio vs min. time: x17.236507057846s
1. Data.List.sort time: 0.012339s; ratio vs min. time: x1.707583725435s
1. Data.Vector.Algorithms.Radix.sort time: 0.007226s; ratio vs min. time: x1s

---------

1. msdSort time: 0.206555s; ratio vs min. time: x21.669639110365s
1. lsdSort time: 0.135591s; ratio vs min. time: x14.224821653378s
1. Data.List.sort time: 0.010211s; ratio vs min. time: x1.071233738984s
1. Data.Vector.Algorithms.Radix.sort time: 0.009532s; ratio vs min. time: x1s

---------

1. msdSort time: 0.257523s; ratio vs min. time: x25.451966791856s
1. lsdSort time: 0.163541s; ratio vs min. time: x16.163372207946s
1. Data.List.sort time: 0.018911s; ratio vs min. time: x1.869045265862s
1. Data.Vector.Algorithms.Radix.sort time: 0.010118s; ratio vs min. time: x1s

---------

1. msdSort time: 0.366789s; ratio vs min. time: x40.279925323962s
1. lsdSort time: 0.207427s; ratio vs min. time: x22.779156600043s
1. Data.List.sort time: 0.014807s; ratio vs min. time: x1.6260707226s
1. Data.Vector.Algorithms.Radix.sort time: 0.009106s; ratio vs min. time: x1s

---------

1. msdSort time: 0.347061s; ratio vs min. time: x32.475063160849s
1. lsdSort time: 0.184119s; ratio vs min. time: x17.22831477496s
1. Data.List.sort time: 0.01939s; ratio vs min. time: x1.814353887901s
1. Data.Vector.Algorithms.Radix.sort time: 0.010687s; ratio vs min. time: x1s