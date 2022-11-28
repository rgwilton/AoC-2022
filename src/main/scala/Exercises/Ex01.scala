package aoc

object Ex01 extends Exercise:
 type ParsedInput = IndexedSeq[Int]

  def parseInput(input: Iterator[String]) = input.asIntegers.toIndexedSeq

  def increasing(x: Seq[Int]) = x(0) < x(1)   

  def part1(using env:Env) = 
    env.input.sliding(2).count(increasing)

  def part2(using env:Env) =
    env.input.sliding(3).map(_.sum).sliding(2).count(increasing)

