package aoc

import scala.collection.mutable.Buffer
import scala.annotation.tailrec

object Ex02 extends Exercise:
 type ParsedInput = Iterator[(Int, Int)]
 override type Common = Array[(Int, Int)]

  def parseInput(input: Iterator[String]) =
    for line <- input yield
      (1 + line(0) - 'A', 1 + line(2) - 'X')
    


  def common(input: ParsedInput) = input.toArray

  def part1(common: Common) = 
    common.collect {
      // a and b represent rock, papper, or scissors, counting from 1.
      case (a, b) if b == a + 1 || (b == 1 && a == 3) => b + 6 // win
      case (a, b) if a == b => b + 3 // draw
      case (a, b) => b // lose
    }.sum

  def part2(common: Common) =
    // a represents rock, paper, scissors.  b represents lose, draw, win, counting from 1.
    common.collect {
      case (a, 3) => 6 + (if a == 3 then 1 else a + 1) // win
      case (a, 2) => a + 3 // draw
      case (a, 1) => if (a == 1) then 3 else a - 1 // lose
    }.sum

