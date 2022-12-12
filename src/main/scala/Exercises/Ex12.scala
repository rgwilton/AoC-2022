package aoc

import scala.collection.mutable

object Ex12 extends Exercise:
  type ParsedInput = (Array[Array[Char]], Int, Int, (Int, Int), (Int, Int))
  override type Common = (Array[Array[Int]], Array[Array[Char]], Int, Int, (Int, Int))

  case class Loc(x: Int, y: Int, steps: Int) extends Ordered[Loc]:
    def compare(that: Loc): Int = that.steps.compare(this.steps)

  def parseInput(input: Iterator[String]) = 
    val grid = input.map { line => line.toCharArray.nn }.toArray

    val depth = grid.length
    val width = grid(0).length
    var start = (0, 0)
    var end = (0, 0) 
    for j <- 0 until depth
        i <- 0 until width do
      if grid(j)(i) == 'S' then 
        grid(j)(i) = 'a'
        start = (i, j)
      if grid(j)(i) == 'E' then 
        grid(j)(i) = 'z'
        end = (i, j)
    (grid, width, depth, start, end)

  def common(input: ParsedInput) =
    val (grid, width, depth, start, end) = input
    val steps = grid.map(_.map(_ => Int.MaxValue))

    val candidates = mutable.PriorityQueue[Loc]()
    def enqueue(x: Int, y: Int, curHeight: Char, curSteps: Int) =
      if steps(y)(x) == Int.MaxValue && curHeight - grid(y)(x) <= 1 then
        steps(y)(x) = curSteps + 1
        candidates.enqueue(Loc(x, y, curSteps + 1))

    def calcNewCandidates(l: Loc) =
      val curH = grid(l.y)(l.x)
      if l.x > 0 then enqueue(l.x - 1, l.y, curH, l.steps)
      if l.x < width - 1 then enqueue(l.x + 1, l.y, curH, l.steps)
      if l.y > 0 then enqueue(l.x, l.y - 1, curH, l.steps)
      if l.y < depth - 1 then enqueue(l.x, l.y + 1, curH, l.steps)

    candidates.enqueue(Loc(end._1, end._2, 0))
    while candidates.nonEmpty do
      val c = candidates.dequeue()
      calcNewCandidates(c)
    (steps, grid, width, depth, start)

  def part1(common: Common) = 
    val (steps, grid, width, depth, start) = common
    steps(start._2)(start._1)

  def part2(common: Common) =
    val (steps, grid, width, depth, start) = common
    var min = Int.MaxValue
    for j <- 0 until depth
        i <- 0 until width do
      if grid(j)(i) == 'a' && steps(j)(i) < min then min = steps(j)(i)
    min
    