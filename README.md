Solutions to [Advent of Code](https://adventofcode.com/) problems in
Haskell. Some problems have also been solved in other languages. All
solutions are noted below.

## Completion

| Year | [Haskell](#Haskell) | [ASM](#ASM) |
|------|---------------------|-------------|
| 2015 | ✓                   |             |
| 2016 | ✓                   |             |
| 2017 | ✓                   |             |
| 2018 | ~12 days            |             |
| 2019 | ~13 days            |             |
| 2020 | ✓                   | 2/25        |

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

### Haskell

Note: 2015-2019 need some cleanup first.

| Day \ Year | 2015 | 2016 | 2017 | 2018 | 2019 | 2020                     |
|------------|------|------|------|------|------|--------------------------|
| 1          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day1/run.hs)  |
| 2          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day2/run.hs)  |
| 3          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day3/run.hs)  |
| 4          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day4/run.hs)  |
| 5          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day5/run.hs)  |
| 6          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day6/run.hs)  |
| 7          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day7/run.hs)  |
| 8          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day8/run.hs)  |
| 9          | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day9/run.hs)  |
| 10         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day10/run.hs) |
| 11         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day11/run.hs) |
| 12         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day12/run.hs) |
| 13         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day13/run.hs) |
| 14         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day14/run.hs) |
| 15         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day15/run.hs) |
| 16         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day16/run.hs) |
| 17         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day17/run.hs) |
| 18         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day18/run.hs) |
| 19         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day19/run.hs) |
| 20         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day20/run.hs) |
| 21         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day21/run.hs) |
| 22         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day22/run.hs) |
| 23         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day23/run.hs) |
| 24         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day24/run.hs) |
| 25         | TBA  | TBA  | TBA  | TBA  | TBA  | [✓](./2020/day25/run.hs) |


### ASM

I only do the ones I feel like for asm.

| Day \ Year | 2020                      |
|------------|---------------------------|
| 1          | [✓](./2020/day1/run.asm)  |
| 2          |                           |
| 3          |                           |
| 4          |                           |
| 5          |                           |
| 6          |                           |
| 7          |                           |
| 8          |                           |
| 9          |                           |
| 10         |                           |
| 11         |                           |
| 12         |                           |
| 13         |                           |
| 14         |                           |
| 15         |                           |
| 16         |                           |
| 17         |                           |
| 18         |                           |
| 19         |                           |
| 20         |                           |
| 21         |                           |
| 22         |                           |
| 23         | [✓](./2020/day23/run.asm) |
| 24         |                           |
| 25         |                           |


## Notes

- For 2020's event I (finally) started adding a library for common
  functions, it can be found [here](./adventofcode).
- The correct version of GHC can be pulled in by simply running
  `nix-shell`. Of course, that requires that you have
  [Nix](https://nixos.org/) installed.
- I'm not sure what version of GHC (+ libraries) I used for earlier
  years. I'll try to clean that up.
- This README is also a WIP...
