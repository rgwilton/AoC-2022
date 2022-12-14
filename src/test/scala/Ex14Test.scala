package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex14Test extends ExTest(Ex14):
  val testcases = Seq(
    TestCase(
   """|498,4 -> 498,6 -> 496,6
      |503,4 -> 502,4 -> 502,9 -> 494,9""", 24, 93),
    TestCase(exInput, 1001, 27976))
