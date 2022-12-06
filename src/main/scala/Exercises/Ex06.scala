package aoc

object Ex06 extends Exercise:
  type ParsedInput = String
  override type Common = String

  def parseInput(input: Iterator[String]) = input.mkString("")

  def common(input: String) = input

  def findMarker(input: String, uniqueChars: Int) =
    input
    .iterator
    .sliding(uniqueChars)
    .zipWithIndex
    .collectFirst {
      case (chars, index) if chars.toSet.size == uniqueChars 
        => index + uniqueChars
    }
    .get

  def part1(input: String) = findMarker(input, 4)
  def part2(input: String) = findMarker(input, 14)