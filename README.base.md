Solutions to [Advent of Code](https://adventofcode.com/) problems in
Haskell. Some problems have also been solved in other languages. All
solutions are noted below.

## Completion

$$$COMPLETION$$$

## Running the Code

Setting up the environment for a specific language, day and year is as
simple as running `./aoc -l LANG DAY YEAR`. Day is the only required
value, the others default to `hs` (Haskell) and 2020 respectively.

```
$ # Haskell
$ ./aoc 23
Setting up day 23 (2020, hs)
Compile and run with:
  ghc -O2 run.hs && time ./run
$ ghc -V
The Glorious Glasgow Haskell Compilation System, version 8.8.4
$ exit
$ # ASM
$ ./aoc -l asm 23
Setting up day 23 (2020, asm)
Compile and run with:
  nasm -felf64 run.asm && ld -o run run.o && time ./run < input.txt
$ nasm -v
NASM version 2.14.02 compiled on Jan  1 1980
```

## Language Completion

$$$LANGUAGE_COMPLETION$$$

## Notes

- For 2020's event I (finally) started adding a library for common
  functions, it can be found [here](./adventofcode).
- The correct version of GHC can be pulled in by simply running
  `nix-shell`. Of course, that requires that you have
  [Nix](https://nixos.org/) installed.
- I'm not sure what version of GHC (+ libraries) I used for earlier
  years. I'll try to clean that up.
- This README is also a WIP...

## TODO

- Use a single cabal project?
