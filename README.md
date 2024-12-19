Solutions to [Advent of Code](https://adventofcode.com/) problems in
Haskell. Some problems have also been solved in other languages. All
solutions are noted below.

## Completion

|  | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|------|------|------|------|------|------|------|------|------|------|------|
| [ASM](#asm) |  |  |  |  |  | 2/25 |  | 1/25 |  |  |
| [Haskell](#haskell) | 24/25 | ✓ | ✓ | 11/25 | 9/25 | ✓ | ✓ | ✓ | 12/25 | 19/25 |
| [Koka](#koka) |  |  |  |  |  |  | 3/25 |  |  |  |
| [Nix](#nix) |  |  |  |  |  |  | 1/25 |  |  |  |

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

## ASM
Solved:
 - [2020, day 1](./2020/day1/run.asm)
 - [2020, day 23](./2020/day23/run.asm)
 - [2022, day 1](./2022/day1/run.asm)

## Haskell
| Day \\ Year | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 | 2021 | 2022 | 2023 | 2024 |
|------|------|------|------|------|------|------|------|------|------|------|
| 1 | [✓](./2015/day1/run.hs) | [✓](./2016/day1/run.hs) | [✓](./2017/day1/run.hs) | [✓](./2018/day1/run.hs) | [✓](./2019/day1/run.hs) | [✓](./2020/day1/run.hs) | [✓](./2021/day1/run.hs) | [✓](./2022/day1/run.hs) | [✓](./2023/day1/run.hs) | [✓](./2024/day1/run.hs) |
| 2 | [✓](./2015/day2/run.hs) | [✓](./2016/day2/run.hs) | [✓](./2017/day2/run.hs) | [✓](./2018/day2/run.hs) | [✓](./2019/day2/run.hs) | [✓](./2020/day2/run.hs) | [✓](./2021/day2/run.hs) | [✓](./2022/day2/run.hs) | [✓](./2023/day2/run.hs) | [✓](./2024/day2/run.hs) |
| 3 | [✓](./2015/day3/run.hs) | [✓](./2016/day3/run.hs) | [✓](./2017/day3/run.hs) | [✓](./2018/day3/run.hs) | [✓](./2019/day3/run.hs) | [✓](./2020/day3/run.hs) | [✓](./2021/day3/run.hs) | [✓](./2022/day3/run.hs) | [✓](./2023/day3/run.hs) | [✓](./2024/day3/run.hs) |
| 4 | [✓](./2015/day4/run.hs) | [✓](./2016/day4/run.hs) | [✓](./2017/day4/run.hs) | [✓](./2018/day4/run.hs) | [✓](./2019/day4/run.hs) | [✓](./2020/day4/run.hs) | [✓](./2021/day4/run.hs) | [✓](./2022/day4/run.hs) | [✓](./2023/day4/run.hs) | [✓](./2024/day4/run.hs) |
| 5 | [✓](./2015/day5/run.hs) | [✓](./2016/day5/run.hs) | [✓](./2017/day5/run.hs) | [✓](./2018/day5/run.hs) | [✓](./2019/day5/run.hs) | [✓](./2020/day5/run.hs) | [✓](./2021/day5/run.hs) | [✓](./2022/day5/run.hs) | [✓](./2023/day5/run.hs) | [✓](./2024/day5/run.hs) |
| 6 | [✓](./2015/day6/run.hs) | [✓](./2016/day6/run.hs) | [✓](./2017/day6/run.hs) | [✓](./2018/day6/run.hs) | [✓](./2019/day6/run.hs) | [✓](./2020/day6/run.hs) | [✓](./2021/day6/run.hs) | [✓](./2022/day6/run.hs) | [✓](./2023/day6/run.hs) | [✓](./2024/day6/run.hs) |
| 7 | [✓](./2015/day7/run.hs) | [✓](./2016/day7/run.hs) | [✓](./2017/day7/run.hs) | [✓](./2018/day7/run.hs) | [✓](./2019/day7/run.hs) | [✓](./2020/day7/run.hs) | [✓](./2021/day7/run.hs) | [✓](./2022/day7/run.hs) | [✓](./2023/day7/run.hs) | [✓](./2024/day7/run.hs) |
| 8 | [✓](./2015/day8/run.hs) | [✓](./2016/day8/run.hs) | [✓](./2017/day8/run.hs) | [✓](./2018/day8/run.hs) |  | [✓](./2020/day8/run.hs) | [✓](./2021/day8/run.hs) | [✓](./2022/day8/run.hs) | [✓](./2023/day8/run.hs) | [✓](./2024/day8/run.hs) |
| 9 | [✓](./2015/day9/run.hs) | [✓](./2016/day9/run.hs) | [✓](./2017/day9/run.hs) | [✓](./2018/day9/run.hs) | [✓](./2019/day9/run.hs) | [✓](./2020/day9/run.hs) | [✓](./2021/day9/run.hs) | [✓](./2022/day9/run.hs) | [✓](./2023/day9/run.hs) | [✓](./2024/day9/run.hs) |
| 10 | [✓](./2015/day10/run.hs) | [✓](./2016/day10/run.hs) | [✓](./2017/day10/run.hs) |  | [✓](./2019/day10/run.hs) | [✓](./2020/day10/run.hs) | [✓](./2021/day10/run.hs) | [✓](./2022/day10/run.hs) | [✓](./2023/day10/run.hs) | [✓](./2024/day10/run.hs) |
| 11 | [✓](./2015/day11/run.hs) | [✓](./2016/day11/run.hs) | [✓](./2017/day11/run.hs) | [✓](./2018/day11/run.hs) |  | [✓](./2020/day11/run.hs) | [✓](./2021/day11/run.hs) | [✓](./2022/day11/run.hs) | [✓](./2023/day11/run.hs) | [✓](./2024/day11/run.hs) |
| 12 | [✓](./2015/day12/run.hs) | [✓](./2016/day12/run.hs) | [✓](./2017/day12/run.hs) | [✓](./2018/day12/run.hs) |  | [✓](./2020/day12/run.hs) | [✓](./2021/day12/run.hs) | [✓](./2022/day12/run.hs) |  | [✓](./2024/day12/run.hs) |
| 13 | [✓](./2015/day13/run.hs) | [✓](./2016/day13/run.hs) | [✓](./2017/day13/run.hs) |  |  | [✓](./2020/day13/run.hs) | [✓](./2021/day13/run.hs) | [✓](./2022/day13/run.hs) |  | [✓](./2024/day13/run.hs) |
| 14 | [✓](./2015/day14/run.hs) | [✓](./2016/day14/run.hs) | [✓](./2017/day14/run.hs) |  |  | [✓](./2020/day14/run.hs) | [✓](./2021/day14/run.hs) | [✓](./2022/day14/run.hs) |  | [✓](./2024/day14/run.hs) |
| 15 | [✓](./2015/day15/run.hs) | [✓](./2016/day15/run.hs) | [✓](./2017/day15/run.hs) |  |  | [✓](./2020/day15/run.hs) | [✓](./2021/day15/run.hs) | [✓](./2022/day15/run.hs) | [✓](./2023/day15/run.hs) | [✓](./2024/day15/run.hs) |
| 16 | [✓](./2015/day16/run.hs) | [✓](./2016/day16/run.hs) | [✓](./2017/day16/run.hs) |  |  | [✓](./2020/day16/run.hs) | [✓](./2021/day16/run.hs) | [✓](./2022/day16/run.hs) |  | [✓](./2024/day16/run.hs) |
| 17 | [✓](./2015/day17/run.hs) | [✓](./2016/day17/run.hs) | [✓](./2017/day17/run.hs) |  |  | [✓](./2020/day17/run.hs) | [✓](./2021/day17/run.hs) | [✓](./2022/day17/run.hs) |  | [✓](./2024/day17/run.hs) |
| 18 | [✓](./2015/day18/run.hs) | [✓](./2016/day18/run.hs) | [✓](./2017/day18/run.hs) |  |  | [✓](./2020/day18/run.hs) | [✓](./2021/day18/run.hs) | [✓](./2022/day18/run.hs) |  | [✓](./2024/day18/run.hs) |
| 19 | [✓](./2015/day19/run.hs) | [✓](./2016/day19/run.hs) | [✓](./2017/day19/run.hs) |  |  | [✓](./2020/day19/run.hs) | [✓](./2021/day19/run.hs) | [✓](./2022/day19/run.hs) |  | [✓](./2024/day19/run.hs) |
| 20 | [✓](./2015/day20/run.hs) | [✓](./2016/day20/run.hs) | [✓](./2017/day20/run.hs) |  |  | [✓](./2020/day20/run.hs) | [✓](./2021/day20/run.hs) | [✓](./2022/day20/run.hs) |  |  |
| 21 | [✓](./2015/day21/run.hs) | [✓](./2016/day21/run.hs) | [✓](./2017/day21/run.hs) |  |  | [✓](./2020/day21/run.hs) | [✓](./2021/day21/run.hs) | [✓](./2022/day21/run.hs) |  |  |
| 22 |  | [✓](./2016/day22/run.hs) | [✓](./2017/day22/run.hs) |  |  | [✓](./2020/day22/run.hs) | [✓](./2021/day22/run.hs) | [✓](./2022/day22/run.hs) |  |  |
| 23 | [✓](./2015/day23/run.hs) | [✓](./2016/day23/run.hs) | [✓](./2017/day23/run.hs) |  |  | [✓](./2020/day23/run.hs) | [✓](./2021/day23/run.hs) | [✓](./2022/day23/run.hs) |  |  |
| 24 | [✓](./2015/day24/run.hs) | [✓](./2016/day24/run.hs) | [✓](./2017/day24/run.hs) |  |  | [✓](./2020/day24/run.hs) | [✓](./2021/day24/run.hs) | [✓](./2022/day24/run.hs) |  |  |
| 25 | [✓](./2015/day25/run.hs) | [✓](./2016/day25/run.hs) | [✓](./2017/day25/run.hs) |  |  | [✓](./2020/day25/run.hs) | [✓](./2021/day25/run.hs) | [✓](./2022/day25/run.hs) |  |  |

## Koka
Solved:
 - [2021, day 1](./2021/day1/run.kk)
 - [2021, day 2](./2021/day2/run.kk)
 - [2021, day 3](./2021/day3/run.kk)

## Nix
Solved:
 - [2021, day 1](./2021/day1/run.nix)


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

