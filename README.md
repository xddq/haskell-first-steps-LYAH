## Learning haskell

- first project where I will read through [this awesome
  book](http://learnyouahaskell.com/) and code the functions used there based on
their description. Trying to get a basic understanding of haskell.

## install haskell toolchain
- if you don't have the haskell toolchain installed, run
```shell
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```
to get ghcup and install the tools.

## play around with these functions
- Everything is stored under ./src/CodeBaseLibrary.hs.
- To execute the functions just open the haskell REPL and load the file.
```shell
ghci
ghci :l /src/CodeBaseLibrary
ghci map' (+1) [1..10]
ghci filter' even [1..10]
ghci filter' (\x -> (mod x 2) == 0) [1..10]
ghci zip' (filter' even  (map (+3) [1..10])) [200..]
ghci applyFuncTwice (+3) 2
```
- If you change code inside /src/CodeBaseLibrary and want to test it, just use
  :r /src/CodeBaseLibrary inside the repl to reload the file.

- compile a cabal project using
```shell
cabal build
cabal run
```

## where to get infos about haskell packages and implementations?
- I currently think [hoogle](https://hoogle.haskell.org/) is decent.
