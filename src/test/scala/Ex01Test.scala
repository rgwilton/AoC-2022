package aoc

import org.junit.Test
import org.junit.Assert.*

class Ex01Test:
  val input = scala.io.Source.fromFile(s"input/input_${Ex01.num}.txt")
  val result = Ex01.run(input.getLines)
  
  @Test def t1(): Unit = 
    assertEquals(result.part1, 71023)
    assertEquals(result.part2, 206289)
