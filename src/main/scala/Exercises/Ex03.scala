package aoc

object Ex03 extends Exercise:
  type ParsedInput = Array[String]
  override type Common = ParsedInput

  def parseInput(input: Iterator[String]) = input.toArray

  def common(parsedInput: ParsedInput) = parsedInput

  def getScore(c: Char) =
    1 + (if c >= 'a' then c - 'a' else 26 + c - 'A')

  def part1(backpacks: Common) = 
    backpacks
    .map { backpack =>
      val (top, bottom) = backpack.splitAt(backpack.length/2)
      getScore(top.intersect(bottom).head)
    }
    .sum

  def part2(backpacks: Common) =
    backpacks
    .grouped(3)
    .map {
      case Array(a, b, c) =>
        getScore(a.intersect(b).intersect(c).head)   
    }
    .sum
