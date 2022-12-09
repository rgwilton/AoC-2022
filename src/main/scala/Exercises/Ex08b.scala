package aoc

import scala.collection.mutable

object Ex08b extends Exercise:
  override val num = "08"

  type Grid = Array[Array[Int]]
  type ParsedInput = Grid
  type Common = Grid

  def parseInput(input: Iterator[String]) =
    input.map { _.toCharArray().nn.map(_.toInt) }.toArray

  def common(input: Grid) = input

  def part1(grid: Grid) =
    val height = grid.length
    val width = grid(0).length
    val visible = mutable.Set[(Int, Int)]()

    def process(x: Int, y: Int, highest: Int) =
      if grid(x)(y) > highest then
        visible += ((x, y))
        grid(x)(y)
      else
        highest

    for y <- 0 until height do
      var highest = -1
      for x <- 0 until width do 
        highest = process(x, y, highest)

      highest = -1
      for x <- (0 until width).reverse do 
        highest = process(x, y, highest)

    for x <- 0 until width do
      var highest = -1
      for y <- 0 until height do
        highest = process(x, y, highest)
      highest = -1
      for y <- (0 until height).reverse do
        highest = process(x, y, highest)

    visible.size

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
