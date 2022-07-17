import std/os/file
import std/os/path

fun part1(input : list<int>) : int
  input.map-peek( fn(c, rest)
    match rest
      Cons(r, _) -> int(r > c)
      Nil -> 0
  ).sum()

fun part2(input : list<int>) : int
  input.map-peek( fn(c, rest)
    match rest.drop(2)
      Cons(r, _) -> int(r > c)
      Nil -> 0
  ).sum()

fun parse-all(input : string) : list<int>
  input.lines.flatmap-maybe(fn (l) parse-int(l))

fun main() {
  val input = parse-all(read-text-file(path("input.txt")))

  println(part1(input))
  println(part2(input))
}
