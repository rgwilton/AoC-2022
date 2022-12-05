package aoc

import scala.collection.mutable.Stack

object Ex05 extends Exercise:
  type ParsedInput = Puzzle
  override type Common = ParsedInput

  val MoveR = """move (\d+) from (\d+) to (\d+)""".r
  case class Move(crates: Int, from: Int, to: Int)
  type Crate = Char
  case class Puzzle(count: Int, stacks: Array[Stack[Crate]], moves: Seq[Move])

  def parseInput(input: Iterator[String]) = 
    val bufInput = input.buffered
    var revCrates = Seq[String]()
    while !bufInput.head.startsWith(" 1 ") do revCrates +:= bufInput.next()
    val count = bufInput.next().split("\\s+").nn.filter(_ != "").length

    // Build stacks by checking for character at each location.    
    val stacks = Array.fill(count + 1)(Stack[Crate]())
    for row <- revCrates
        i <- 0 until count do
      val ch = row(1 + (i * 4))
      if (ch != ' ') stacks(i + 1).push(ch)

    val moves = 
      for case MoveR(count, from, to) <- bufInput yield
        Move(count.toInt, from.toInt, to.toInt)
    Puzzle(count, stacks, moves.toSeq)

  def common(puzzle: Puzzle) = puzzle

  def moveCrate(stacks: Array[Stack[Crate]], from: Int, to: Int) =
    val ch = stacks(from).pop()
    stacks(to).push(ch)
  def part1(puzzle: Puzzle) = 
    val stacks = puzzle.stacks.map(_.clone)
    for move <- puzzle.moves
        i <- 1 to move.crates do
      moveCrate(stacks, move.from, move.to)
    stacks.tail.map(_.head).mkString("")

  def part2(puzzle: Puzzle) =
    val stacks = puzzle.stacks.map(_.clone)

    // Move crates via a temp stack so that original order is preserved.
    for move <- puzzle.moves do
      for i <- 1 to move.crates do
        moveCrate(stacks, move.from, 0)

      for i <- 1 to move.crates do
        moveCrate(stacks, 0, move.to)

    stacks.tail.map(_.head).mkString("")