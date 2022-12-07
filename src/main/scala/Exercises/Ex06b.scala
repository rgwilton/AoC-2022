package aoc

import scala.collection.mutable

// Faster version of Ex 6 that makes uses an array to maintain a count of the number of each character seen.
object Ex06b extends Exercise:
  override val num = "06"
  type ParsedInput = String
  override type Common = String

  def parseInput(input: Iterator[String]) = input.mkString("")

  def common(input: String) = input

  def findMarker(input: String, uniqueChars: Int) =
    val charCounts = Array.fill(26)(0)
    val chars = input.toArray
    for i <- 0 until uniqueChars do
      charCounts(chars(i) - 'a') += 1
    var count = charCounts.count(_ > 0)

    (uniqueChars until chars.length)
    .iterator
    .map {
      case i =>
        val removeIndex = chars(i - uniqueChars) - 'a'
        charCounts(removeIndex) -= 1
        if charCounts(removeIndex) == 0 then count -= 1

        val addIndex = chars(i) - 'a'
        charCounts(addIndex) += 1
        if charCounts(addIndex) == 1 then count += 1

        (count, i + 1)
    }.filter {
      case (size, j) => size == uniqueChars
    }
    .map(_._2)
    .next()

  def part1(input: String) = findMarker(input, 4)
  def part2(input: String) = findMarker(input, 14)