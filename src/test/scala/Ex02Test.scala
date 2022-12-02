package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex02Test extends ExTest(Ex02):
  val testcases = Seq(
    TestCase(
    """|A Y
       |B X
       |C Z""", 15, 12),
    TestCase(exInput, 11873, 12014)
  )