package aoc

import scala.collection.mutable
import scala.annotation.tailrec

object Ex09 extends Exercise:
  type ParsedInput = Seq[(Char, Int)]
  override type Common = ParsedInput
  class Pos(var x: Int, var y: Int):
    def move(a: Int, b: Int) = 
      x += a
      y += b

  def parseInput(input: Iterator[String]) = 
    input.map { line =>
      val Array(dir, steps) = line.split(" ").nn
      (dir.nn.head, steps.nn.toInt)
    }.toSeq

  def common(input: ParsedInput) = input

  def simulateRope(len: Int, moves: Seq[(Char, Int)]) =
    val visited = mutable.Set((0,0))
    val pos = Array.fill(len)(Pos(0,0))
    def head = pos(0)
    def tail = pos(len - 1)
    for (dir, steps) <- moves
        s <- 1 to steps do
          // Move head
          dir match
            case 'R' => head.move(1, 0)
            case 'L' => head.move(-1, 0)
            case 'U' => head.move(0, 1)
            case 'D' => head.move(0, -1)

          // Move rest of rope
          for p <- 1 until len do
            val cur = pos(p)
            val prev = pos(p - 1)
            if (cur.x - prev.x).abs >= 2 ||
              (cur.y - prev.y).abs >= 2 then

              if cur.x < prev.x then cur.move(1, 0)
              else if cur.x > prev.x then cur.move(-1, 0)
              if cur.y < prev.y then cur.move(0, 1)
              else if cur.y > prev.y then cur.move(0, -1)

          visited += ((tail.x, tail.y))

    visited.size    
  def part1(common: Common) = simulateRope(2, common)    

  def part2(common: Common) = simulateRope(10, common)
