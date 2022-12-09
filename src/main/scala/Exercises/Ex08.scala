package aoc

import scala.collection.mutable

object Ex08 extends Exercise:
  type Grid = Array[Array[Int]]
  type ParsedInput = Grid
  type Common = Grid

  def parseInput(input: Iterator[String]) =
    input.map { _.toCharArray().nn.map(_.toInt) }.toArray

  def common(input: Grid) = input

  def part1(grid: Grid) =
    val height = grid.length
    val width = grid(0).length
    val visible = Array.fill(width, height)(0)

    for y <- 0 until height do
      visible(0)(y) = 1
      var highest = grid(0)(y)
      for x <- 0 until width do
        if grid(x)(y) > highest then
          visible(x)(y) = 1
          highest = grid(x)(y)

      visible(width - 1)(y) = 1
      highest = grid(width - 1)(y)
      for x <- width - 1 to 0 by -1 do
        if grid(x)(y) > highest then
          visible(x)(y) = 1
          highest = grid(x)(y)

    for x <- 0 until width do
      visible(x)(0) = 1
      var highest = grid(x)(0)
      for y <- 0 until height do
        if grid(x)(y) > highest then
          visible(x)(y) = 1
          highest = grid(x)(y)

      visible(x)(height - 1) = 1
      highest = grid(x)(height - 1)
      for y <- height - 1 to 0 by -1 do
        if grid(x)(y) > highest then
          visible(x)(y) = 1
          highest = grid(x)(y)

      // x.map { x => (x, grid(x)(y))}
      // .sliding(2)
      // .takeWhile { case Seq((a,b),(c,d)) => b <= d }
      // .foreach { case Seq((a,b),(c,d)) => visible(c)(y) = 1 }

    // for y <- 0 until grid.length do
    //   println(visible(y).mkString(""))

    var count = 0
    for x <- 0 until width
        y <- 0 until height do
          count += visible(x)(y)
    count

    
  def part2(grid: Grid) =
    val height = grid.length
    val width = grid(0).length
    val scenicScores = 
      for x <- 0 until width
         y <- 0 until height yield
        val tgt = grid(x)(y)

        val xp = 
           (x + 1 to width - 1)
           .iterator
           .find { p => grid(p)(y) >= tgt }
           .getOrElse(width - 1)
           - x

        val xm = 
           x - 
           (x - 1 to 0 by -1)
           .iterator
           .find { p => grid(p)(y) >= tgt }
           .getOrElse(0)

        val yp = 
           (y + 1 to height - 1)
           .iterator
           .find { p => grid(x)(p) >= tgt }
           .getOrElse(height - 1)
           - y

        val ym = 
           y - 
           (y - 1 to 0 by -1)
           .iterator
           .find { p => grid(x)(p) >= tgt }
           .getOrElse(0)

        xp * xm * yp * ym

    scenicScores.max
