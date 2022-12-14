package aoc

import scala.collection.mutable
import fastparse._

object Ex14 extends Exercise:
  type ParsedInput = Seq[Line]
  override type Common = (Array[Array[Char]], Int, Int, Int)

  case class Pos(x: Int, y: Int)
  case class Line(positions: Seq[Pos])

  object Parser:
    import NoWhitespace._
    def number[p: P]: P[Int] = P( CharsWhileIn("0-9").!.map(_.toInt) )
    def pos[p: P]: P[Pos] = P(number ~/ "," ~/ number).map { Pos.apply }
    def line[p: P]: P[Line] = P(pos.rep(sep = " -> ")).map { Line.apply }
    def inputP[p: P] = P(line.rep(sep = "\n") ~ End)

  def parseInput(input: Iterator[String]) = 
    val res = parse(input.mkString("\n"), Parser.inputP(_))
    res.get.value

  def common(lines: ParsedInput) =
    val yPositions = 
      for l <- lines
          pos <- l.positions yield pos.y
    val maxY = yPositions.max + 2
    val xPositions = 
      for l <- lines
          pos <- l.positions yield pos.x
    val minX = xPositions.min min (500 - maxY - 1)
    val maxX = xPositions.max max (500 + maxY + 1)
    val cave = Array.fill(maxY + 1,maxX + 1)('.')

    // Draw lines.
    for line <- lines do
      line.positions.sliding(2).foreach {
        case Seq(Pos(x1, y1), Pos(x2, y2)) =>
          if x1 == x2 then
            val start = y1 min y2
            val end = y1 max y2
            for y <- start to end do
              cave(y)(x1) = '#'
          else
            val start = x1 min x2
            val end = x1 max x2
            for x <- start to end do
              cave(y1)(x) = '#'       
      }

    // Draw base
    for x <- 0 until maxX do 
      cave(maxY)(x) = '#'

    (cave, minX, maxX, maxY)

  def part1(common: Common) = 
    val (startCave, minX, maxX, maxY) = common
    val cave = startCave.map(_.clone)

    // Simulate sand.
    var count = 0
    var starts = List(Pos(500, 0))
    val maxRow = maxY - 2
    while starts.head.y != maxRow do
      val sX = starts.head.x
      val sY = starts.head.y
      if cave(sY + 1)(sX) == '.' then
        starts ::= Pos(sX, sY + 1)
      else if cave(sY + 1)(sX - 1) == '.' then 
        starts ::= Pos(sX - 1, sY + 1)
      else if cave(sY + 1)(sX + 1) == '.' then
        starts ::= Pos(sX + 1, sY + 1)
      else
        cave(sY)(sX) = 'o'
        count += 1
        starts = starts.tail      
    count

  def part2(common: Common) =
    val (startCave, minX, maxX, maxY) = common
    val cave = startCave.map(_.clone)

    // Simulate sand.
    var count = 0
    var starts = List(Pos(500, 0))
    while starts.nonEmpty do
      val sX = starts.head.x
      val sY = starts.head.y
      if cave(sY + 1)(sX) == '.' then
        starts ::= Pos(sX, sY + 1)
      else if cave(sY + 1)(sX - 1) == '.' then 
        starts ::= Pos(sX - 1, sY + 1)
      else if cave(sY + 1)(sX + 1) == '.' then
        starts ::= Pos(sX + 1, sY + 1)
      else
        cave(sY)(sX) = 'o'
        count += 1
        starts = starts.tail

    count
