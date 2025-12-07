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
