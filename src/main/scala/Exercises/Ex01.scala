package aoc

import scala.collection.mutable.Buffer
import scala.annotation.tailrec

object Ex01 extends Exercise:
  type ParsedInput = IndexedSeq[IndexedSeq[Int]]
  override type Common = IndexedSeq[Int]

  def parseInput(input: Iterator[String]) =
    input.splitByEmptyLine.map(_.asIntegers)

  def common(input: ParsedInput) =
    input.map(_.sum)

  def part1(common: Common) = 
    common.max

  def part2(common: Common) =
    common.sorted.takeRight(3).sum

