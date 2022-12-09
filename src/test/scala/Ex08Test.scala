package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex08Test extends ExTest(Ex08):
  val testcases = Seq(
    TestCase(
 """|30373
    |25512
    |65332
    |33549
    |35390""", 21, 8),
    TestCase(exInput, 1849, 201600))
