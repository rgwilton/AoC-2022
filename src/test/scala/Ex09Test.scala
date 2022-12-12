package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex09Test extends ExTest(Ex09):
  val testcases = Seq(
    TestCase(
 """|R 4
    |U 4
    |L 3
    |D 1
    |R 4
    |D 1
    |L 5
    |R 2""", 13, 1),
    TestCase(exInput, 6175, 2578))
