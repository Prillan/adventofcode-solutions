import std/os/file
import std/os/path
import std/text/parse

fun part1(input)
  1

fun part2(input)
  2

fun pline()
  pint()

fun parse-all(input : string)
  input.lines.flatmap-maybe(fn (l)
    l.slice.parse(pline).maybe
  )

fun main() {
  val input = parse-all(read-text-file(path("input.txt")))

  println(part1(input))
  println(part2(input))
}
