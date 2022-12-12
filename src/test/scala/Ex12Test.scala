package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex12Test extends ExTest(Ex12):
  val testcases = Seq(
    TestCase(
   """|Sabqponm
      |abcryxxl
      |accszExk
      |acctuvwj
      |abdefghi""", 31, 29),
    TestCase(exInput, 394, 388))
