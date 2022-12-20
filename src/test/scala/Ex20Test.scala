package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex20Test extends ExTest(Ex20):
  val testcases = Seq(
    TestCase(
   """|1
      |2
      |-3
      |3
      |-2
      |0
      |4""", 3L, 1623178306L),
    TestCase(exInput, 15297L, 2897373276210L))
