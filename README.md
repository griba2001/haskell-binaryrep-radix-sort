## Radix sort of numbers and numerically indexed data types, based on binary representation of primitive types

With msd prefix you have parallel Most significant digit radix sort.

With lsd prefix you have Least significant digit radix sort.

Actually the LSD version runs slightly faster.

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

Sorting floats (Float, Double) or records with a floating-point index:

```haskell
    import Data.List.RadixSort.Base (lsdSort, lsdSortBy)

    import Data.List as L
    import System.Random
    import Control.Monad
    import Data.Ord

    data FRec = FRec {fieldF:: Float} deriving (Eq, Show)

    floatExample = do

        listF1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Float,100))
        let listF2 = L.map FRec listF1

        print listF1

        putStrLn "\n"
        print $ lsdSort listF1

        putStrLn "\n"
        print $ lsdSortBy fieldF listF2

    main = floatExample
```

Sorting integers (type IntN) or records with an IntN index:

```haskell
    import Data.List.RadixSort.Base (lsdSort, lsdSortBy)

    import Data.List as L
    import System.Random
    import Control.Monad
    import Data.Int
    import Data.Ord

    data IRec = IRec {fieldI:: Int32} deriving (Eq, Show)

    intExample = do

        listI1 <- replicateM 10 $ getStdRandom (randomR ((-100)::Int32,100))
        let listI2 = L.map IRec listI1

        print listI1

        putStrLn "\n"
        print $ lsdSort listI1

        putStrLn "\n"
        print $ lsdSortBy fieldI listI2


    main = intExample
```

Sorting naturals (type WordN) or records with a WordN index:

```haskell
    import Data.List.RadixSort.Base (lsdSort, lsdSortBy)

    import Data.List as L
    import System.Random
    import Control.Monad
    import Data.Word
    import Data.Ord

    data WRec = WRec {fieldW:: Word32} deriving (Eq, Show)

    wordExample = do

        listN1 <- replicateM 10 $ getStdRandom (randomR (0::Word32,100))
        let listN2 = L.map WRec listN1

        print listN1
        putStrLn "\n"
        print $ lsdSort listN1

        putStrLn "\n"
        print $ lsdSortBy fieldW listN2

    main = wordExample

