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

It uses

* The __vector__ library to implement vectors
* The __Data.Sequence__ module to implement queues with constant O(1) access to both ends
* The __dlist__ package (difference lists) that have O(1) append costs to collect ordered sequences.

Actually both MSD and LSD methods run much slower than Data.List.sort (the test includes a benchmark).


Comparison of times sorting a 10000 size list of Floats (replicated 5 times)

msdSort time: 0.238941s; ratio vs min. time: x13.200430915419s
lsdSort time: 0.436473s; ratio vs min. time: x24.113198165847s
Data.List.sort time: 0.018101s; ratio vs min. time: x1s

msdSort time: 0.336847s; ratio vs min. time: x25.565194292653s
lsdSort time: 0.676234s; ratio vs min. time: x51.323163327261s
Data.List.sort time: 0.013176s; ratio vs min. time: x1s

msdSort time: 0.404574s; ratio vs min. time: x34.727381974248s
lsdSort time: 0.4368s; ratio vs min. time: x37.493562231759s
Data.List.sort time: 0.01165s; ratio vs min. time: x1s

msdSort time: 0.23087s; ratio vs min. time: x15.643718661065s
lsdSort time: 0.395681s; ratio vs min. time: x26.811288792519s
Data.List.sort time: 0.014758s; ratio vs min. time: x1s

msdSort time: 0.247786s; ratio vs min. time: x18.722024933887s
lsdSort time: 0.413432s; ratio vs min. time: x31.237778617302s
Data.List.sort time: 0.013235s; ratio vs min. time: x1s


Comparison of times sorting a 10000 size list of Int32 (replicated 5 times)

msdSort time: 0.209426s; ratio vs min. time: x19.563381597384s
lsdSort time: 0.111159s; ratio vs min. time: x10.383839327417s
Data.List.sort time: 0.010705s; ratio vs min. time: x1s

msdSort time: 0.192351s; ratio vs min. time: x20.348143446524s
lsdSort time: 0.113413s; ratio vs min. time: x11.997566909975s
Data.List.sort time: 0.009453s; ratio vs min. time: x1s

msdSort time: 0.192336s; ratio vs min. time: x19.759194575713s
lsdSort time: 0.113663s; ratio vs min. time: x11.676905691391s
Data.List.sort time: 0.009734s; ratio vs min. time: x1s

msdSort time: 0.199737s; ratio vs min. time: x19.911972884059s
lsdSort time: 0.118777s; ratio vs min. time: x11.840992921941s
Data.List.sort time: 0.010031s; ratio vs min. time: x1s

msdSort time: 0.233371s; ratio vs min. time: x19.019641401792s
lsdSort time: 0.120724s; ratio vs min. time: x9.838956805215s
Data.List.sort time: 0.01227s; ratio vs min. time: x1s
