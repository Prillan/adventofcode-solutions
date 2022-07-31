import std/os/file
import std/os/path
import std/text/parse

fun lshow(xs: list<list<int>>)
  xs.show-list(fn (l) l.show-list(show))

fun most-common-bin(xs: list<int>): int
  val s = xs.sum()
  val l = xs.length()

  if 2 * s >= l then 1
  else 0

fun least-common-bin(xs: list<int>): int
  if xs.most-common-bin() == 0 then 1
  else 0

fun counts(xs : list<list<int>>): <exn> list<int>
  val cs = xs.foldl1 fn (a, b)
    a.zipwith(b, (+))
  val l = xs.length()

  cs.map fn (v)
    if 2 * v >= l then 1
    else 0

fun toint2(xs: list<int>): int
  xs.reverse.map-indexed(fn (i, v) v * exp2(i)).sum()

fun flip(xs: list<int>): list<int>
  xs.map fn (v)
    if v == 1 then 0
    else 1

fun part1(input)
  val cs = counts(input)
  val gamma = cs.toint2()
  val epsilon = cs.flip().toint2()

  gamma * epsilon


fun locate(xs: list<list<int>>, selector: (list<int> -> int))
  fun locate'(remaining: list<list<int>>): <div> list<int>
    match remaining
      Nil -> Nil
      Cons(x, Nil) -> x
      _ ->
        val v = remaining.flatmap-maybe(head).selector()
        val rest = remaining.flatmap-maybe fn (row)
          match row
            Cons(_, Nil) -> Nothing
            Cons(h, tail) -> if h == v then Just(tail)
                             else Nothing
            Nil -> Nothing

        Cons(v, rest.locate'())

  xs.locate'()

fun part2(input)
  val o2 = input.locate(most-common-bin).toint2()
  val co2 = input.locate(least-common-bin).toint2()

  o2 * co2

fun pline()
  many(psingle)

fun psingle()
  val c = one-of("01")
  if c == '0' then 0
  else 1

fun parse-all(input : string): list<list<int>>
  input.lines.flatmap-maybe(fn (l)
    l.slice.parse(pline).maybe
  ).filter(is-cons)

fun main() {
  val input = parse-all(read-text-file(path("input.txt")))

  println(part1(input))
  println(part2(input))
}
