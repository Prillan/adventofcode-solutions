import std/os/file
import std/os/path
import std/text/parse

fun part1(input : list<instruction>) : int
  val pos = input.foldl((0,0), fn (pos, i)
    val (h, d) = pos
    match i.direction
      Forward -> (h + i.value, d)
      Backward -> (h - i.value, d)
      Up -> (h, d - i.value)
      Down -> (h, d + i.value)
  )
  pos.fst * pos.snd

fun part2(input : list<instruction>) : int
  val pos = input.foldl((0, 0, 0), fn (pos, i)
    val (a, h, d) = pos
    match i.direction
      Forward -> (a, h + i.value, d + a * i.value)
      Up -> (a - i.value, h, d)
      Down -> (a + i.value, h, d)
      Backward -> pos
  )
  pos.snd * pos.thd

type direction
  Forward
  Backward
  Up
  Down

struct instruction
  direction : direction
  value : int

fun pdirection() : parse direction
  [
    { pstring("forward"); Forward },
    { pstring("backward"); Backward },
    { pstring("up"); Up },
    { pstring("down"); Down },
  ].choose()

fun pinstruction() : parse instruction
  val d = pdirection()
  whitespace()
  val v = pint()
  Instruction(d, v)

fun parse-all(input : string) : list<instruction>
  input.lines.flatmap-maybe(fn (l)
    l.slice.parse(pinstruction).maybe
  )

fun main() {
  val input = parse-all(read-text-file(path("input.txt")))

  println(part1(input))
  println(part2(input))
}
