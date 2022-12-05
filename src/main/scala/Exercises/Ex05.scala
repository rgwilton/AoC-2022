package aoc

import scala.collection.mutable.Stack

object Ex05 extends Exercise:
  type ParsedInput = Puzzle
  override type Common = ParsedInput

  case class Move(count: Int, from: Int, to: Int)
  type Crate = Char
  case class Puzzle(count: Int, stacks: Array[Stack[Crate]], moves: Seq[Move])

  def parseInput(input: Iterator[String]) = 
    val bufInput = input.buffered
    var revCrates = Seq[String]()
    while !bufInput.head.startsWith(" 1 ") do revCrates +:= bufInput.next()
    val count = bufInput.next().split("\\s+").nn.filter(_ != "").length

    // Build count + 1 stacks by checking for character at each location
    // Stack 0 is for a temporary stack.
    val stacks = Array.fill(count + 1)(Stack[Crate]())
    for row <- revCrates
        i <- 0 until count do
      val ch = row(1 + (i * 4))
      if (ch != ' ') stacks(i + 1).push(ch)

    val MoveR = """move (\d+) from (\d+) to (\d+)""".r
    val moves = 
      for case MoveR(count, from, to) <- bufInput yield
        Move(count.toInt, from.toInt, to.toInt)
    Puzzle(count, stacks, moves.toSeq)

  def common(puzzle: Puzzle) = puzzle

  def moveCrate(stacks: Array[Stack[Crate]], from: Int, to: Int, count: Int) =
    for i <- 1 to count do
      val crate = stacks(from).pop()
      stacks(to).push(crate)
    
  def part1(puzzle: Puzzle) = 
    val stacks = puzzle.stacks.map(_.clone)

    for move <- puzzle.moves do
      moveCrate(stacks, move.from, move.to, move.count)

    stacks.tail.map(_.head).mkString("")

  def part2(puzzle: Puzzle) =
    val stacks = puzzle.stacks.map(_.clone)

    // Move crates via a temp stack (location 0) so that original order is preserved.
    for move <- puzzle.moves do
      moveCrate(stacks, move.from, to = 0, move.count)
      moveCrate(stacks, from = 0, move.to, move.count)

    stacks.tail.map(_.head).mkString("")